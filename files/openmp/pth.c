#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#define PTH_NUM_THREADS 4

void *HelloElectron(void *threadid)
{
    printf("Hello electron\n");
    pthread_exit(NULL);
}

int main (int argc, char *argv[])
{
   pthread_t threads[PTH_NUM_THREADS];
   int rc;
   long t;
   for(t=0; t<PTH_NUM_THREADS; t++){
    rc = pthread_create(&threads[t], NULL, HelloElectron, (void *)t);
    if (rc) exit(-1);
   }
   pthread_exit(NULL);
   return 0;
}

