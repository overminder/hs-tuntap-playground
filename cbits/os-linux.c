#include "cbits/os.h"

#include <unistd.h>
#include <netinet/ip.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <linux/if.h>
#include <linux/if_tun.h>

#include <stdio.h>
#include <string.h>

#include "cbits/os-unix.inc.c"

int
tunAlloc(int isTun, char *dev)
{
    // The interface request
    struct ifreq ifr;
    int fd;
    int err;
    const char *cloneDev = "/dev/net/tun";

    if ((fd = open(cloneDev, O_RDWR)) < 0) {
        perror("open");
        return fd;
    }

    ZERO_OUT(ifr);
    // IFF_TUN or IFF_TAP, and maybe IFF_NO_PI
    if (isTun) {
        ifr.ifr_flags = IFF_TUN | IFF_NO_PI;
    }
    else {
        ifr.ifr_flags = IFF_TAP | IFF_NO_PI;
    }

    if (*dev) {
        strncpy(ifr.ifr_name, dev, IFNAMSIZ);
    }

    if ((err = ioctl(fd, TUNSETIFF, &ifr)) < 0) {
        perror("ioctl");
        close(fd);
        return err;
    }

    strcpy(dev, ifr.ifr_name);

    return fd;
}

int
tunBringUp(const char *dev) {
    struct ifreq ifr;
    int err;
    ZERO_OUT(ifr);
    strncpy(ifr.ifr_name, dev, IFNAMSIZ);
    if ((err = interfaceIoctl(SIOCGIFFLAGS, &ifr)) < 0) {
        perror("tunBringUp: ioctl SIOCGIFFLAGS");
        return err;
    }

    ifr.ifr_flags |= IFF_UP;
    if ((err = interfaceIoctl(SIOCSIFFLAGS, &ifr)) < 0) {
        perror("tunBringUp: ioctl set IFF_UP");
        return err;
    }

    return 0;
}

MAKE_SET_INET_ADDR(tunSetIp,   ifr_addr,    SIOCSIFADDR)
MAKE_SET_INET_ADDR(tunSetMask, ifr_netmask, SIOCSIFNETMASK)
MAKE_SET_INET_ADDR(tunSetDst,  ifr_dstaddr, SIOCSIFDSTADDR)

int
tunSetIpMaskDst(const char *dev, uint32_t ip, uint32_t mask, uint32_t dst) {
    int err;
    if ((err = tunSetIp(dev, ip)) < 0) {
        return err;
    }
    if ((err = tunSetMask(dev, mask)) < 0) {
        return err;
    }
    if ((err = tunSetDst(dev, dst)) < 0) {
        return err;
    }
    return 0;
}

