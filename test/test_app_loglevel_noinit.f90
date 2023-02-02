program test_app_fortran

      use app

      implicit none
      integer :: ier
      INTEGER, DIMENSION(-1:1, -1:2) :: A

      ! This call to app_log will not print but will cause
      ! App to be initialized and 
      ! write(app_msg,55) 'This is an info message'  ! print message and return file type
      ! call app_log(APP_INFO,app_msg)

      !
      ! Because App is not initialized, the following call
      ! to app_log(APP_INFO) will cause a call to App_InitEnv();
      ! which will reset the App->LogLevel[APP_MAIN] to APP_WARNING
      !
      ier = app_loglevelno(APP_INFO)

      write(app_msg,55) 'This is an INFO message after a call to app_loglevelno(APP_INFO)'  ! print message and return file type
      call app_log(APP_INFO,app_msg)

      write(app_msg,55) 'This is an error message'  ! print message and return file type
      call app_log(APP_ERROR,app_msg)

 55   format(a,a60)

end
