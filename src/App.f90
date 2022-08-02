module app
    use, intrinsic :: iso_c_binding
    implicit none
interface

    ! 
    ! Bindings using C adapters
    !

!   TApp *App_Init(int Type,char* Name,char* Version,char* Desc,char* Stamp);
    type(C_PTR) FUNCTION app_init(type,name,version,desc,stamp) BIND(C,name="App_Init")
        use, intrinsic :: iso_c_binding
        implicit none
        integer(C_INT), value :: type
        type(C_PTR), value :: name
        type(C_PTR), value :: version
        type(C_PTR), value :: desc
        type(C_PTR), value :: stamp
    end FUNCTION

!   void  App_Free(void);
    SUBROUTINE app_free() BIND(C, name="App_Free")
        use, intrinsic :: iso_c_binding
    end SUBROUTINE

!   void  App_Start(void);
    SUBROUTINE app_start() BIND(C, name="App_Start")
        use, intrinsic :: iso_c_binding
    end SUBROUTINE

!   int   App_End(int Status);
    integer(C_INT) FUNCTION app_end(status) BIND(C, name="App_End")
        use, intrinsic :: iso_c_binding
        implicit none
        integer(C_INT), value :: status
    end FUNCTION

!    void  App_Log(TApp_LogLevel Level,const char *Format,...);
!    SUBROUTINE app_log(level,const char *Format,...)iris_model_finalize(iris) BIND(C, name="Iris_Model_Finalize")
!        use, intrinsic :: iso_c_binding
!        implicit none
!        integer(C_INT), value :: level
!        type(C_PTR), value :: iris
!    end SUBROUTINE

!   void  App_Progress(float Percent,const char *Format,...);

!   void  App_LogOpen(void);
    SUBROUTINE app_logopen() BIND(C, name="App_LogOpen")
        use, intrinsic :: iso_c_binding
    end SUBROUTINE

!   void  App_LogClose(void);
    SUBROUTINE app_logclose() BIND(C, name="App_LogClose")
        use, intrinsic :: iso_c_binding
    end SUBROUTINE

!   int   App_LogLevel(char *Val);
    integer(C_INT) FUNCTION app_loglevel(level) BIND(C, name="App_LogLevel")
        use, intrinsic :: iso_c_binding
        implicit none
        integer(C_INT), value :: level
    end FUNCTION

!    int   App_ParseArgs(TApp_Arg *AArgs,int argc,char *argv[],int Flags);
!    int   App_ParseInput(void *Def,char *File,TApp_InputParseProc *ParseProc);

!   int   App_ParseBool(char *Param,char *Value,char *Var);
    integer(C_INT) FUNCTION app_parsebool(param,value,var) BIND(C, name="App_ParseBool")
        use, intrinsic :: iso_c_binding
        implicit none
        type(C_PTR), value :: param
        type(C_PTR), value :: value
        type(C_PTR), intent(out) :: var
    end FUNCTION


!   int   App_ParseDate(char *Param,char *Value,time_t *Var);
    integer(C_INT) FUNCTION app_parsedate(param,value,var) BIND(C, name="App_ParseDate")
        use, intrinsic :: iso_c_binding
        implicit none
        type(C_PTR), value :: param
        type(C_PTR), value :: value
        integer(C_LONG), intent(out) :: var
    end FUNCTION

!   int   App_ParseDateSplit(char *Param,char *Value,int *Year,int *Month,int *Day,int *Hour,int *Min);
    integer(C_INT) FUNCTION app_parsedatesplit(param,value,year,month,day,hour,min) BIND(C, name="App_ParseDateSplit")
        use, intrinsic :: iso_c_binding
        implicit none
        type(C_PTR), value :: param
        type(C_PTR), value :: value
        integer(C_INT), intent(out) :: year
        integer(C_INT), intent(out) :: month
        integer(C_INT), intent(out) :: day
        integer(C_INT), intent(out) :: hour
        integer(C_INT), intent(out) :: min
    end FUNCTION

!   int   App_ParseCoords(char *Param,char *Value,double *Lat,double *Lon,int Index);
    integer(C_INT) FUNCTION app_parsecoords(param,value,lat,lon,index) BIND(C, name="App_ParseCoords")
        use, intrinsic :: iso_c_binding
        implicit none
        type(C_PTR), value :: param
        type(C_PTR), value :: value
        integer(C_INT), intent(out) :: lat
        integer(C_INT), intent(out) :: lon
        integer(C_INT), value :: index
    end FUNCTION

!   void  App_SeedInit(void);
    SUBROUTINE app_seedinit() BIND(C, name="App_SeedInit")
        use, intrinsic :: iso_c_binding
    end SUBROUTINE

!   char* App_ErrorGet(void);
    type(C_PTR) FUNCTION app_errorget() BIND(C, name="App_ErrorGet")
        use, intrinsic :: iso_c_binding
    end FUNCTION

!   int   App_ThreadPlace(void);
    integer(C_INT) FUNCTION app_threadplace() BIND(C, name="App_ThreadPlace")
        use, intrinsic :: iso_c_binding
    end FUNCTION

!   void  App_Trap(int Signal);
    SUBROUTINE app_trap(signal) BIND(C, name="App_Trap")
        use, intrinsic :: iso_c_binding
        integer(C_INT), value :: signal
    end SUBROUTINE

!   int   App_IsDone(void); 
    integer(C_INT) FUNCTION app_isdone() BIND(C, name="App_IsDone")
        use, intrinsic :: iso_c_binding
    end FUNCTION

!   int   App_IsMPI(void);
    integer(C_INT) FUNCTION app_ismpi() BIND(C, name="App_IsMPI")
        use, intrinsic :: iso_c_binding
    end FUNCTION

!   int   App_IsOMP(void);
    integer(C_INT) FUNCTION app_isomp() BIND(C, name="App_IsOMP")
        use, intrinsic :: iso_c_binding
    end FUNCTION

!   int   App_IsSingleNode(void);
    integer(C_INT) FUNCTION app_issinglenode() BIND(C, name="App_IsSingleNode")
        use, intrinsic :: iso_c_binding
    end FUNCTION

!   int   App_IsAloneNode(void);
    integer(C_INT) FUNCTION app_isalonenode() BIND(C, name="App_IsAloneNode")
        use, intrinsic :: iso_c_binding
    end FUNCTION

!   int   App_NodeGroup();
    integer(C_INT) FUNCTION app_nodegroup() BIND(C, name="App_NodeGroup")
        use, intrinsic :: iso_c_binding
    end FUNCTION
end interface
end module