#define ZERO_OUT(s) \
    memset(&s, 0, sizeof(s));

#define INIT_IFR(ifrVar, devName) \
    struct ifreq ifrVar; \
    int err; \
    ZERO_OUT(ifrVar); \
    strncpy(ifrVar.ifr_name, devName, IFNAMSIZ)

static int
interfaceIoctl(int cmd, struct ifreq *ifr) {
    int ret;
    int sock;

    sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock < 0) {
        perror("socket");
        return sock;
    }
    ret = ioctl(sock, cmd, ifr);
    close(sock);
    return ret;
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

