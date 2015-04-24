#ifndef ERL_COMMUNICATION_H_
#define ERL_COMMUNICATION_H_

#include <stdint.h>
#include <stdbool.h>

typedef uint8_t byte;

bool wait_for_data(int timeoutMS);
int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);

#endif
