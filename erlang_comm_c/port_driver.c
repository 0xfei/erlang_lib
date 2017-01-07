#include <stdio.h>
#include <string.h>
#include "erl_driver.h"

typedef struct {
	ErlDrvPort port;
} data;

static ErlDrvData drv_start(ErlDrvPort port, char* buf)
{
	data* d = (data*)driver_alloc(sizeof(data));
	d->port = port;
	return (ErlDrvData)d;
}

static void drv_stop(ErlDrvData handle)
{
	driver_free(handle);
}

void split_string(char* s, char** s1, char** s2)
{
	*s1 = s;
	*s2 = s + strlen(s) + 1;	
}

static void drv_output(ErlDrvData handle, char* buf,
		ErlDrvSizeT len)
{
	data* d = (data*)handle;
	char *s1, *s2;

	int res = 0, fn = buf[0];
	
	if (fn == 1) {
		res = strlen(buf + 1);
    } else if (fn == 2) {
        split_string(buf + 1, &s1, &s2);
        res = strcmp(s1,s2);
    }

	driver_output(d->port, (char*)&res, 1);
}

ErlDrvEntry driver_entry = {
	NULL,
	drv_start,
	drv_stop,
	drv_output,
	NULL,
	NULL,
	"port_driver",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	ERL_DRV_EXTENDED_MARKER,
	ERL_DRV_EXTENDED_MAJOR_VERSION,
	ERL_DRV_EXTENDED_MINOR_VERSION,
	0,
	NULL,
	NULL,
	NULL
};

DRIVER_INIT(port_driver)
{
	return &driver_entry;
}

