/*****
                          sysdep.h  -  description
                             -------------------
    begin                : Mon 16 February 2004
    copyright            : (C) 2004 by Ron Dilley
    email                : ron.dilley@uberadmin.com
 *****/

/*****
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *****/

#ifndef __SYSDEP_H__
#define __SYSDEP_H__

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdarg.h>
#include <errno.h>
#include <pwd.h>
#include <grp.h>
#define _GNU_SOURCE
#include <time.h>

#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
#endif

#ifdef HAVE_DIRENT_H
# include <dirent.h>
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif

#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif

#if !defined(HAVE_NETINET_IF_ETHER_H) && defined(HAVE_LINUX_IF_ETHER_H)
# include <linux/if_ether.h>
# define ether_header ethhdr
# define ether_type h_proto
# define ETHERTYPE_IP ETH_P_IP
#endif

#ifdef HAVE_MEMORY_H
# include <memory.h>
#endif

#ifdef HAVE_NDIR_H
# include <ndir.h>
#endif

#ifdef HAVE_NETDB_H
# include <netdb.h>
#endif

#ifdef HAVE_NETINET_IN_H
# include <netinet/in.h>
#endif

#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif

#ifdef HAVE_NET_IF_H
# include <net/if.h>
#endif

#ifdef HAVE_NETINET_IF_ETHER_H
# include <netinet/if_ether.h>
#endif

#ifdef HAVE_NETINET_IN_SYSTM_H
# include <netinet/in_systm.h>
#endif

#ifdef HAVE_NETINET_IP_H
# include <netinet/ip.h>
#endif

#ifdef HAVE_NETINET_TCP_H
# include <netinet/tcp.h>
#endif

/*
#include <netpacket/packet.h>
*/

#ifdef HAVE_NETINET_UDP_H
# include <netinet/udp.h>
#endif

#ifdef HAVE_NETINET_IP_ICMP_H
# include <netinet/ip_icmp.h>
#endif

#ifdef HAVE_NETINET_ETHER_H
# include <netinet/ether.h>
#endif

#ifdef HAVE_PATHS_H
# include <paths.h>
#endif

#ifdef HAVE_SIGNAL_H
# include <signal.h>
#endif

#ifdef HAVE_STANDARDS_H
# include <standards.h>
#endif

#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#endif

#ifdef HAVE_STRINGS_H
# include <strings.h>
#endif

#ifdef HAVE_SYSLOG_H
# include <syslog.h>
#endif

#ifdef HAVE_SYS_BITYPES_H
# include <sys/bitypes.h>
#endif

#ifdef HAVE_SYS_DIR_H
# include <sys/dir.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif

#ifdef HAVE_SYS_NDIR_H
# include <sys/ndir.h>
#endif

#ifdef HAVE_SYS_PARAM_H
# include <sys/param.h>
#endif

#ifdef HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#ifdef HAVE_SYS_TYPES_H
/*@-skipposixheaders@*/
# include <sys/types.h>
/*@=skipposixheaders@*/
#endif

#ifdef HAVE_SYS_SOCKIO_H
# include <sys/sockio.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_GETOPT_H
# include <getopt.h>
#endif

#ifdef HAVE_VFORK_H
# include <vfork.h>
#endif

#ifdef HAVE_LIBNET_H
/*@-skipposixheaders@*/
# include <libnet.h>
/*@-skipposixheaders@*/
#endif

#ifdef HAVE_PCAP_H
/*@-skipposixheaders@*/
# include <pcap.h>
/*@-skipposixheaders@*/
#endif

#ifndef MAXNAMELEN
# define MAXNAMELEN 1024
#endif

#endif /* __SYSDEP_H__ */

