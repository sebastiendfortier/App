/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : 
 * Fichier      : System.c
 * Creation     : Avril 2006 - J.P. Gauthier
 *
 * Description  : Fonctions generales d'utilites courantes.
 *
 * Remarques    :
 *
 * License      :
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation,
 *    version 2.1 of the License.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the
 *    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 *    Boston, MA 02111-1307, USA.
 *
 *=========================================================
 */

#include <time.h>
#include <sys/time.h>
#include <sys/types.h>
#include <math.h>

#include "System.h"
#include "App.h"
#ifdef HAVE_RMN
   #include "rmn.h"
   #include "rmn/rpnmacros.h"
#endif

static char SYSTEM_STRING[256];

/*----------------------------------------------------------------------------
 * Nom      : <System_IsBigEndian>
 * Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Verifier l'endianness.
 *
 * Parametres  :
 *
 * Retour:
 *  <True>     : Vrai si la machine est big-endianness
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int System_IsBigEndian(void) {
   short w=0x4321;

   if ((*(char*)&w)!=0x21)
     return(1);
   else
     return(0);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_ByteOrder>
 * Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Verifier l'endianness.
 *
 * Parametres  :
 *
 * Retour:
 *  <Order>   : Endianness (SYS_LITTLE_ENDIAN ou SYS_BIG_ENDIAN)
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int System_ByteOrder(void) {
   short w=0x0001;
   char *byte=(char*)&w;

   return(byte[0]?SYS_LITTLE_ENDIAN:SYS_BIG_ENDIAN);
}

/*----------------------------------------------------------------------------
 * Nom      : <SSystem_BitCount>
 * Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Compte du nombre de bit a 1.
 *
 * Parametres  :
 *   <N>       : Bytes a verifier
 *
 * Retour:
 *  <Nombre>   : Nombre de bit a 1
 *
 * Remarques :
 *   This is HACKMEM 169, as used in X11 sources. Source: MIT AI Lab memo, late 1970?s.
 *   works for 32-bit numbers only, fix last line for 64-bit numbers
 *
 *----------------------------------------------------------------------------
 */
int System_BitCount(unsigned int N) {

   register unsigned int tmp;

   tmp = N - ((N>>1) & 033333333333) - ((N>>2) & 011111111111);
   return ((tmp + (tmp >> 3)) & 030707070707) % 63;
}

/*----------------------------------------------------------------------------
 * Nom      : <System_TimeValSubtract>
 * Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Difference entre 2 temps en structure timeval
 *
 * Parametres  :
 *   <Result>  : Difference
 *   <T0>      : Temps 0
 *   <T1>      : Temps 1
 *
 * Retour:
 *  <Neg>      : Negatif (1=negatif,0=positif);
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
double System_TimeValSubtract(struct timeval *Result,struct timeval *T0,struct timeval *T1) {

   int nsec;

   /* Perform the carry for the later subtraction by updating y. */
   if (T0->tv_usec<T1->tv_usec) {
      nsec=(T1->tv_usec-T0->tv_usec)/1000000+1;
      T1->tv_usec-=1000000*nsec;
      T1->tv_sec+=nsec;
   }
   if (T0->tv_usec-T1->tv_usec>1000000) {
      nsec=(T1->tv_usec-T0->tv_usec)/1000000;
      T1->tv_usec+=1000000*nsec;
      T1->tv_sec-=nsec;
   }

   Result->tv_sec =T0->tv_sec-T1->tv_sec;
   Result->tv_usec=T0->tv_usec-T1->tv_usec;

   /* Return 1 if result is negative. */
   return(Result->tv_sec+((double)Result->tv_usec)*1e-6);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_DateTime2Seconds>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Convertir une date en secondes.
 *
 * Parametres  :
 *  <YYYYMMDD> : Date
 *  <HHMMSS>   : Heure
 *  <GMT>      : GMT (GMT ou local)
 *
 * Retour:
 *  <Sec>      : Secondes
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
time_t System_DateTime2Seconds(int YYYYMMDD,int HHMMSS,int GMT) {

   struct tm date;

   extern time_t timezone;

   date.tm_sec=fmod(HHMMSS,100);       /*seconds apres la minute [0,61]*/
   HHMMSS/=100;
   date.tm_min=fmod(HHMMSS,100);       /*minutes apres l'heure [0,59]*/
   HHMMSS/=100;
   date.tm_hour=HHMMSS;                /*heures depuis minuit [0,23]*/

   date.tm_mday=fmod(YYYYMMDD,100);    /*jour du mois [1,31]*/
   YYYYMMDD/=100;
   date.tm_mon=fmod(YYYYMMDD,100)-1;   /*mois apres Janvier [0,11]*/
   YYYYMMDD/=100;
   date.tm_year=YYYYMMDD-1900;         /*annee depuis 1900*/
   date.tm_isdst=0;                    /*Flag de l'heure avancee*/

   /* Force GMT and set back to original TZ after*/
   if (GMT) {
      return(mktime(&date)-timezone);
   } else {
      return(mktime(&date));
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <System_Seconds2DateTime>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Convertir une date en secondes.
 *
 * Parametres  :
 *  <Sec>      : Secondes
 *  <YYYYMMDD> : Date
 *  <HHMMSS>   : Heure
 *  <GMT>      : GMT (GMT ou local)
 *
 * Retour:
 *  <Sec>      : Secondes
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
time_t System_Seconds2DateTime(time_t Sec,int *YYYYMMDD,int *HHMMSS,int GMT) {

   struct tm *tsec;

   if (GMT) {
      tsec=gmtime(&Sec);
   } else {
      tsec=localtime(&Sec);
   }

   *YYYYMMDD=(tsec->tm_year+1900)*10000+(tsec->tm_mon+1)*100+tsec->tm_mday;
   *HHMMSS=tsec->tm_hour*10000+tsec->tm_min*100+tsec->tm_sec;

   return(Sec);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_Julian2Stamp>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Convertir une date julienne date stamp.
 *
 * Parametres  :
 *  <Year>     : Annee
 *  <Day>      : Jour
 *  <Time>     : Heure
 *
 * Retour:
 *  <Stamp>    : RPN Date stamp
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int System_Julian2Stamp(int Year,int Day,int Time) {

   struct tm date;
   int       stamp=0,op,d,t;

#ifdef HAVE_RMN
   date.tm_sec=fmod(Time,100);   /*seconds apres la minute [0,61]*/
   Time/=100.0;
   date.tm_min=fmod(Time,100);   /*minutes apres l'heure [0,59]*/
   Time/=100.0;
   date.tm_hour=(Day-1)*24+Time; /*heures depuis minuit [0,23]*/
   date.tm_mday=1;               /*jour du mois [1,31]*/
   date.tm_mon=0;                /*mois apres Janvier [0,11]*/
   date.tm_year=Year-1900;       /*annee depuis 1900*/
   date.tm_isdst=0;             /*Flag de l'heure avancee*/
   mktime(&date);

   /*yyyymmdd hhmmss00*/
   op=3;
   d=(date.tm_year+1900)*10000+(date.tm_mon+1)*100+date.tm_mday;
   t=date.tm_hour*1000000+date.tm_min*10000+date.tm_sec*100;
   f77name(newdate)(&stamp,&d,&t,&op);
#else
   App_Log(APP_ERROR,"%s: Need RMNLIB\n",__func__);
#endif

   return(stamp);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_Date2Julian>
 * Creation : Octobre 2008- J.P. Gauthier - CMC/CMOE
 *
 * But      : Convertir une date en date juillienne.
 *
 * Parametres  :
 *  <Year>     : Annee
 *  <Month>    : Mois
 *  <Day>      : Jour
 *
 * Retour:
 *  <Jul>     : Date julienne
 *
 * Remarques :
 *   Ce code provient de xearth (sunpos.c) de kirk johnson july 1993
 *----------------------------------------------------------------------------
*/
double System_Date2Julian(int Year,int Month,int Day){
   int    A, B, C, D;

   if ((Month==1) || (Month==2)) {
      Year-=1;
      Month+=12;
   }

   A=Year/100;
   B=2-A+(A/4);
   C=365.25*Year;
   D=30.6001*(Month+1);

   return(B+C+D+Day+1720994.5);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_StampDecode>
 * Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Decoder un stamp dans ses composantes anne,mois,jou,heure,minute,seconde.
 *
 * Parametres  :
 *  <Stamp>    : Date stampe RPN.
 *  <YYYY>     : Annee
 *  <MM>       : Mois
 *  <DD>       : Jour
 *  <H>        : Heure
 *  <M>        : Minute
 *  <S>        : Seconde
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void System_StampDecode(int Stamp,int *YYYY,int *MM,int *DD,int *H,int *M,int *S) {

   int op=-3,date,time;

#ifdef HAVE_RMN
   f77name(newdate)(&Stamp,&date,&time,&op);

   *YYYY=date/10000;
   *DD=date-((*YYYY)*10000);
   *MM=(*DD)/100;
   *DD-=((*MM)*100);

   *H=time/1000000;
   *S=time-(*H)*1000000;
   *M=(*S)/10000;
   *S-=(*M)*10000;
   *S/=100;
#else
   App_Log(APP_ERROR,"%s: Need RMNLIB\n",__func__);
#endif
}

void System_StampEncode(int *Stamp,int YYYY,int MM,int DD,int H,int M,int S) {

   int op=3,date,time;

#ifdef HAVE_RMN
   date=YYYY*10000+MM*100+DD;
   time=H*1000000+M*10000+S*100;
   f77name(newdate)(Stamp,&date,&time,&op);
#else
   App_Log(APP_ERROR,"%s: Need RMNLIB\n",__func__);
#endif
}

/*----------------------------------------------------------------------------
 * Nom      : <System_Seconds2Stamp>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Encoder des secondes en date stamp.
 *
 * Parametres  :
 *  <Sec>      : Secondes
 *
 * Retour:
 *  <Stamp>    : RPN Date stamp
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int System_Seconds2Stamp(long Sec) {

   int         stamp=0,date,time,op=3;
   struct tm  *tsec;

#ifdef HAVE_RMN
   if (!Sec) {
      return(0);
   }
   tsec=gmtime(&Sec);
   date=(tsec->tm_year+1900)*10000+(tsec->tm_mon+1)*100+tsec->tm_mday;
   time=tsec->tm_hour*1000000+tsec->tm_min*10000+tsec->tm_sec*100;

   f77name(newdate)(&stamp,&date,&time,&op);
#else
   App_Log(APP_ERROR,"%s: Need RMNLIB\n",__func__);
#endif
   return(stamp);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_Stamp2Seconds>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Decoder un date stamp en secondes.
 *
 * Parametres  :
 *  <Stamp>    : RPN Date stamp
 *
 * Retour:
 *  <Sec>      : Secondes
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
long System_Stamp2Seconds(int Stamp) {

   int           yyyy,mm,dd,hh,nn,ss;
   struct tm     tdate;

   extern time_t timezone;

   if (!Stamp) {
      return(0);
   }
   
   System_StampDecode(Stamp,&yyyy,&mm,&dd,&hh,&nn,&ss);

   tdate.tm_sec=ss;           /*seconds apres la minute [0,61]*/
   tdate.tm_min=nn;           /*minutes apres l'heure [0,59]*/
   tdate.tm_hour=hh;          /*heures depuis minuit [0,23]*/
   tdate.tm_mday=dd;          /*jour du mois [1,31]*/
   tdate.tm_mon=mm-1;         /*mois apres Janvier [0,11]*/
   tdate.tm_year=yyyy-1900;   /*annee depuis 1900*/
   tdate.tm_isdst=0;          /*Flag de l'heure avancee*/

   /* Force GMT and set back to original TZ after*/
   return(mktime(&tdate)-timezone);
}

/*----------------------------------------------------------------------------
 * Nom      : <System_StampFormat>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Formatter un datestamp RPN en chaine de data.
 *
 * Parametres  :
 *  <Stamp>    : RPN Date stamp
 *  <Buf>      : Buffer de retour contenant la date formate
 *  <Format>   : Format de date (see strftime)
 *
 * Retour:
 *  <char*>    : Pointeur sur la date formatee
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
char* System_StampFormat(int Stamp,char *Buf,char *Format) {

   char         *buf;
   int           yyyy,mm,dd,hh,nn,ss;
   struct tm     tdate;

   extern time_t timezone;

   buf=Buf?Buf:SYSTEM_STRING;

   System_StampDecode(Stamp,&yyyy,&mm,&dd,&hh,&nn,&ss);

   tdate.tm_sec=ss-timezone;  /*seconds apres la minute [0,61]*/
   tdate.tm_min=nn;           /*minutes apres l'heure [0,59]*/
   tdate.tm_hour=hh;          /*heures depuis minuit [0,23]*/
   tdate.tm_mday=dd;          /*jour du mois [1,31]*/
   tdate.tm_mon=mm-1;         /*mois apres Janvier [0,11]*/
   tdate.tm_year=yyyy-1900;   /*annee depuis 1900*/
   tdate.tm_isdst=0;          /*Flag de l'heure avancee*/

   mktime(&tdate);
   strftime(buf,256,Format,&tdate);

   return(buf);
}
