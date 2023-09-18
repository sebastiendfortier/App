#ifndef MPMD_H__
#define MPMD_H__

#include <mpi.h>

#include "App.h"

typedef enum {
    MPMD_NONE            = 0,
    MPMD_GEM_ID          = 1,
    MPMD_GEM_IOSERVER_ID = 2,
    MPMD_IRIS_ID         = 3,
    MPMD_NEMO_ID        = 4,

    MPMD_TEST1_ID = 1001,
    MPMD_TEST2_ID = 1002,
    MPMD_TEST3_ID = 1003,
    MPMD_TEST4_ID = 1004,
    MPMD_TEST5_ID = 1005,
} TApp_MpmdId;

TApp* Mpmd_Init(const TApp_MpmdId component_id);
MPI_Comm Mpmd_Get_shared_comm(const int32_t* components, const int32_t num_components);
MPI_Fint Mpmd_Get_shared_comm_f(const int32_t* components, const int32_t num_components);
MPI_Comm Mpmd_Get_own_comm(void);
MPI_Fint Mpmd_Get_own_comm_f(void);
int32_t Mpmd_has_component(const TApp_MpmdId component_id);
void Mpmd_Finalize(/*TApp* app*/);
const char* component_id_to_name(const TApp_MpmdId component_id);

#endif // MPMD_H__
