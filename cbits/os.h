#ifndef OS_H
#define OS_H

#include <stdint.h>

int tunAlloc(int isTun, char *dev);
int tunBringUp(const char *dev);
int tunSetIpMaskDst(const char *dev, uint32_t inetAddr, uint32_t mask,
    uint32_t dstAddr);
int tunGetMtu(const char *dev, int *mtu);

#endif
