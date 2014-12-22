#define ZERO_OUT(s) \
    memset(&s, 0, sizeof(s));

#define INIT_IFR(ifrVar, devName) \
    struct ifreq ifrVar; \
    int err; \
    ZERO_OUT(ifrVar); \
    strncpy(ifrVar.ifr_name, devName, IFNAMSIZ)

static int
interfaceIoctl(unsigned long cmd, struct ifreq *ifr) {
    int err;
    int sock;

    if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        perror("socket");
        return sock;
    }
    if ((err = ioctl(sock, cmd, ifr)) < 0) {
        perror("interfaceIoctl.ioctl");
        return err;
    }
    if ((err = close(sock)) < 0) {
        perror("interfaceIoctl.close");
        return err;
    }
    return 0;
}

int
tunGetMtu(const char *dev, int *mtu)
{
    INIT_IFR(ifr, dev);

    if ((err = interfaceIoctl(SIOCGIFMTU, &ifr)) < 0) {
        perror("tunGetMtu: ioctl");
        return err;
    }

    *mtu = ifr.ifr_mtu;
    return 0;
}

