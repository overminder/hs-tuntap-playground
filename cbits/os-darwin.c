#include "cbits/os.h"

#include <unistd.h>
#include <netinet/ip.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <net/if.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cbits/os-unix.inc.c"

int
tunAlloc(int isTun, char *dev)
{
    const char *devFormat;
    char devFullName[15];
    int fd = -1;
    int devNum = 0;

    if (isTun) {
        devFormat = "/dev/tun%d";
    }
    else {
        devFormat = "/dev/tap%d";
    }

    do {
        snprintf(devFullName, sizeof(devFullName) - 1, devFormat, devNum++);
    } while (devNum <= 255 && (fd = open(devFullName, O_RDWR)) < 0);

    if (fd < 0) {
        perror("tunAlloc: open");
        return -1;
    }

    strncpy(dev, devFullName + 5 /* skip "/dev/" */, IFNAMSIZ);

    return fd;
}

int
tunBringUp(const char *dev)
{
    struct ifreq ifr;
    int err;
    memset(&ifr, 0, sizeof ifr);
    strncpy(ifr.ifr_name, dev, IFNAMSIZ);

    if ((err = interfaceIoctl(SIOCGIFFLAGS, &ifr)) < 0) {
        perror("tunBringUp: ioctl SIOCGIFFLAGS");
        return err;
    }

    ifr.ifr_flags |= IFF_UP;
    ifr.ifr_flags |= IFF_RUNNING;
    if ((err = interfaceIoctl(SIOCSIFFLAGS, &ifr)) < 0) {
        perror("tunBringUp: ioctl set IFF_UP IFF_RUNNING");
        return err;
    }

    return 0;
}

int
tunSetIpMaskDst(const char *dev, uint32_t ip, uint32_t mask, uint32_t dst)
{
    struct ifaliasreq ifra;
    struct sockaddr_in *sin;
    int err;

    ZERO_OUT(ifra);
    strncpy(ifra.ifra_name, dev, IFNAMSIZ);

    sin = (struct sockaddr_in *) &ifra.ifra_addr;
    sin->sin_family = AF_INET;
    sin->sin_len = sizeof(ifra.ifra_addr);
    sin->sin_addr.s_addr = ip;

    sin = (struct sockaddr_in *) &ifra.ifra_mask;
    sin->sin_family = AF_INET;
    sin->sin_len = sizeof(ifra.ifra_mask);
    sin->sin_addr.s_addr = mask;

    // XXX: broadaddr === dstaddr?
    sin = (struct sockaddr_in *) &ifra.ifra_broadaddr;
    sin->sin_family = AF_INET;
    sin->sin_len = sizeof(ifra.ifra_broadaddr);
    sin->sin_addr.s_addr = dst;

    if ((err = interfaceIoctl(SIOCAIFADDR, (struct ifreq *) &ifra)) < 0) {
        perror("tunSetIpMaskDst: SIOCAIFADDR");
        return err;
    }

    return 0; 
}

