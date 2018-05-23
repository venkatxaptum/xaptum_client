#include <ei.h>
#include <errno.h>
#include <stdio.h>
#include <memory.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <net/if.h>
#include <linux/if.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <linux/rtnetlink.h>

#define ERR_READ 10
#define ERR_READ_HEADER 11
#define ERR_PACKET_SIZE 12

#define BUFFSIZE 8192

// little helper to parsing message using netlink macroses
static void parseRtattr(struct rtattr *tb[], int max, struct rtattr *rta, int len)
{
  memset(tb, 0, sizeof(struct rtattr *) * (max + 1));

  while (RTA_OK(rta, len)) {  // while not end of the message
    if (rta->rta_type <= max) {
      tb[rta->rta_type] = rta; // read attr
    }
    rta = RTA_NEXT(rta,len);    // get next attr
  }
}

static void write_packet(char *buf, int sz, FILE *fd) {
  uint8_t hd[4];
  hd[0] = (sz >> 24) & 0xff;
  hd[1] = (sz >> 16) & 0xff;
  hd[2] = (sz >> 8) & 0xff;
  hd[3] = sz & 0xff;
  fwrite(hd, 1, 4, fd);
  
  fwrite(buf, 1, sz, fd);
  fflush(fd);
}

static size_t read_bytes(unsigned char *buf, size_t max, FILE *fd) {
  size_t n;
  n = fread(buf, 1, max, fd) ;
  if ((n == 0) && !feof(fd)) {
    exit(ERR_READ);
  }
  return n;
}

static void read_packet(unsigned char *buf, size_t max, FILE *fd) {
  size_t n, sz;
  uint8_t hd[4];
  n = read_bytes(hd, 4, fd) ;
  if (n == 0 && feof(fd)) exit(EXIT_SUCCESS);
  if (n != 4) exit(ERR_READ_HEADER);
  sz = (hd[0] << 24) + (hd[1] << 16) + (hd[2] << 8) + hd[3];
  if (sz > max) {
    exit(ERR_PACKET_SIZE) ;
  }
  n = read_bytes(buf, sz, fd) ;
  if (n != sz) {
    exit(ERR_READ);
  }
}

int main()
{
  char buffer[BUFFSIZE];
  int fd = socket(AF_NETLINK, SOCK_RAW, NETLINK_ROUTE);   // create netlink socket

  if (fd < 0) {
    sprintf(buffer, "Failed to create netlink socket: %s\n", (char*)strerror(errno));
    write_packet(buffer, strlen(buffer), stdout);
    return 1;
  }

  struct sockaddr_nl  local;  // local addr struct
  char buf[8192];             // message buffer
  struct iovec iov;           // message structure
  iov.iov_base = buf;         // set message buffer as io
  iov.iov_len = sizeof(buf);  // set size

  memset(&local, 0, sizeof(local));

  local.nl_family = AF_NETLINK;       // set protocol family
  local.nl_groups =   RTMGRP_LINK | RTMGRP_IPV4_IFADDR | RTMGRP_IPV4_ROUTE;   // set groups we interested in
  local.nl_pid = getpid();    // set out id using current process id

  // initialize protocol message header
  struct msghdr msg;  
  {
    msg.msg_name = &local;                  // local address
    msg.msg_namelen = sizeof(local);        // address size
    msg.msg_iov = &iov;                     // io vector
    msg.msg_iovlen = 1;                     // io size
  }   

  if (bind(fd, (struct sockaddr*)&local, sizeof(local)) < 0) {     // bind socket
    sprintf(buffer, "Failed to bind netlink socket: %s\n", (char*)strerror(errno));
    write_packet(buffer, strlen(buffer), stdout);
    close(fd);
    return 1;
  }   

  ei_x_buff eix;
  // read and parse all messages from the
  while (1) {
    ssize_t status = recvmsg(fd, &msg, MSG_DONTWAIT);

    //  check status
    if (status < 0) {
      if (errno == EINTR || errno == EAGAIN)
	{
	  usleep(250000);
	  continue;
	}

      sprintf(buffer, "Failed to read netlink: %s\n", (char*)strerror(errno));
      write_packet(buffer, strlen(buffer), stdout);
      continue;
    }

    if (msg.msg_namelen != sizeof(local)) { // check message length, just in case
      sprintf(buffer, "Invalid length of the sender address struct\n");
      write_packet(buffer, strlen(buffer), stdout);
      continue;
    }

    // message parser
    struct nlmsghdr *h;

    for (h = (struct nlmsghdr*)buf; status >= (ssize_t)sizeof(*h); ) {   // read all messagess headers
      int len = h->nlmsg_len;
      int l = len - sizeof(*h);
      char *ifName;

      if ((l < 0) || (len > status)) {
	sprintf(buffer, "Invalid message length: %i\n", len);
	write_packet(buffer, strlen(buffer), stdout);
	continue;
      }

      // now we can check message type
      if ((h->nlmsg_type == RTM_NEWROUTE) || (h->nlmsg_type == RTM_DELROUTE)) { // some changes in routing table
	sprintf(buffer, "Routing table was changed\n");
	write_packet(buffer, strlen(buffer), stdout);
      } else {    // in other case we need to go deeper
	char *ifUpp;
	char *ifRunn;
	struct ifinfomsg *ifi;  // structure for network interface info
	struct rtattr *tb[IFLA_MAX + 1];

	ifi = (struct ifinfomsg*) NLMSG_DATA(h);    // get information about changed network interface

	parseRtattr(tb, IFLA_MAX, IFLA_RTA(ifi), h->nlmsg_len);  // get attributes
                
	if (tb[IFLA_IFNAME]) {  // validation
	  ifName = (char*)RTA_DATA(tb[IFLA_IFNAME]); // get network interface name
	}

	if (ifi->ifi_flags & IFF_UP) { // get UP flag of the network interface
	  ifUpp = (char*)"up";
	} else {
	  ifUpp = (char*)"down";
	}

	if (ifi->ifi_flags & IFF_RUNNING) { // get RUNNING flag of the network interface
	  ifRunn = (char*)"running";
	} else {
	  ifRunn = (char*)"not_running";
	}

	char ifAddress[256];    // network addr
	struct ifaddrmsg *ifa; // structure for network interface data
	struct rtattr *tba[IFA_MAX+1];

	ifa = (struct ifaddrmsg*)NLMSG_DATA(h); // get data from the network interface

	parseRtattr(tba, IFA_MAX, IFA_RTA(ifa), h->nlmsg_len);

	if (tba[IFA_LOCAL]) {
	  inet_ntop(AF_INET, RTA_DATA(tba[IFA_LOCAL]), ifAddress, sizeof(ifAddress)); // get IP addr
	}

	switch (h->nlmsg_type) { // what is actually happenned?
	case RTM_DELADDR:
	  ei_x_new_with_version(&eix);
	  ei_x_encode_tuple_header(&eix, 2);
	  ei_x_encode_atom(&eix, "deladdr");
	  ei_x_encode_string(&eix, ifName);
	  write_packet(eix.buff, eix.buffsz, stdout);
	  ei_x_free(&eix);
	  break;

	case RTM_DELLINK:
	  ei_x_new_with_version(&eix);
	  ei_x_encode_tuple_header(&eix, 2);
	  ei_x_encode_atom(&eix, "dellink");
	  ei_x_encode_string(&eix, ifName);
	  write_packet(eix.buff, eix.buffsz, stdout);
	  ei_x_free(&eix);
	  break;

	case RTM_NEWLINK:
	  ei_x_new_with_version(&eix);
	  ei_x_encode_tuple_header(&eix, 4);
	  ei_x_encode_atom(&eix, "newlink");
	  ei_x_encode_string(&eix, ifName);
	  ei_x_encode_atom(&eix, ifUpp);
	  ei_x_encode_atom(&eix, ifRunn);
	  write_packet(eix.buff, eix.buffsz, stdout);
	  ei_x_free(&eix);
	  break;

	case RTM_NEWADDR:
	  ei_x_new_with_version(&eix);
	  ei_x_encode_tuple_header(&eix, 3);
	  ei_x_encode_atom(&eix, "newaddr");
	  ei_x_encode_string(&eix, ifName);
	  ei_x_encode_string(&eix, ifAddress);
	  write_packet(eix.buff, eix.buffsz, stdout);
	  ei_x_free(&eix);
	  break;
	}
      }

      status -= NLMSG_ALIGN(len); // align offsets by the message length, this is important

      h = (struct nlmsghdr*)((char*)h + NLMSG_ALIGN(len));    // get next message
    }

    usleep(250000); // sleep for a while
  }

  close(fd);  // close socket

  return 0;
}
