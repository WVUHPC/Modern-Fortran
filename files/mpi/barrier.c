
#include <stdio.h>
#include "mpi.h"

int main(int argc, char** argv){

int my_PE_num, numbertoreceive, numbertosend=4,index, result=0;

MPI_Status status;
MPI_Init(&argc, &argv);
MPI_Comm_rank(MPI_COMM_WORLD, &my_PE_num);

if (my_PE_num==0)
for (index=1; index<my_PE_num; index++)
MPI_Send( &numbertosend, 1,MPI_INT, index, 10,MPI_COMM_WORLD);
else{
MPI_Recv( &numbertoreceive, 1, MPI_INT, 0, 10, MPI_COMM_WORLD, &status);
result = numbertoreceive * my_PE_num;
}
for (index=1; index<my_PE_num; index++){
MPI_Barrier(MPI_COMM_WORLD);
if (index==my_PE_num) printf("PE %d's result is %d.\n", my_PE_num, result);
}
if (my_PE_num==0){
for (index=1; index<my_PE_num; index++){
MPI_Recv( &numbertoreceive, 1,MPI_INT,index,10, MPI_COMM_WORLD, &status);
result += numbertoreceive;
}
printf("Total is %d.\n", result);
}
else
MPI_Send( &result, 1, MPI_INT, 0, 10, MPI_COMM_WORLD);
MPI_Finalize();
return 0;
}

