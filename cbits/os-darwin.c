#include "cbits/os.h"

#include <stdio.h>
#include <string.h>

#include "cbits/os-unix.inc.c"

int
tunAlloc(int isTun, char *dev)
{
    struct ifreq ifr;
    const char *devFormat;
    char devFullName[15];
    int fd;
    int devNum = 0;

    ZERO_OUT(ifr);

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

    strncpy(ifr.ifr_name, devFullName + 5 /* skip "/dev/" */, IFNAMSIZE);
    strncpy(dev, ifr.ifr_name, IFNAMSIZE);

    return fd;
}

int
tunBringUp(const char *dev)
{
    struct ifreq ifr;
    int err;
    ZERO_OUT(ifr);
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
tunSetIpAndMask(const char *dev, uint32_t ip, uint32_t mask)
{
    struct ifaliasreq ifra;
    struct sockaddr_in *sin;
    int err;

    ZERO_OUT(ifra);
    strncpy(ifra.ifra_name, dev, IFNAMSIZE);

    sin = (struct sockaddr_in *) &ifra.ifra_addr;
    sin->sin_family = AF_INET;
    sin->sin_len = sizeof(ifra.ifra_addr);
    sin->sin_addr.s_addr = ip;

    sin = (struct sockaddr_in *) &ifra.ifra_mask;
    sin->sin_family = AF_INET;
    sin->sin_len = sizeof(ifra.ifra_mask);
    sin->sin_addr.s_addr = mask;

    if ((err = interfaceIoctl(SIOCSIFADDR, (ifreq *) &ifra)) < 0) {
        perror("tunSetIpAndMask: SIOCAIFADDR");
        return err;
    }

    return 0; 
}

