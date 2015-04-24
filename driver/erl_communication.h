#ifndef ERL_COMMUNICATION_H_
#define ERL_COMMUNICATION_H_

#include <stdint.h>

typedef uint8_t byte;

int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);

#endif
