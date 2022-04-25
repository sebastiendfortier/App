/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : 
 * Fichier      : str.c
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include </usr/include/regex.h>

char* strpath(char *Path,char *File) {

   char *c;
   char *new;

   c=strrchr(Path,'/');
   if (!c) c=Path;
   new=(char*)calloc(strlen(File)+(c-Path)+2,1);

   strncpy(new,Path,(c-Path));
   strcat(new,"/");
   strcat(new,File);
   
   return(new);
}

char* strcatalloc(char *StrTo,char *StrFrom) {

   if (StrFrom) {
      if (StrTo) {
         StrTo=(char*)realloc(StrTo,strlen(StrTo)+strlen(StrFrom)+1);
         strcat(StrTo,StrFrom);
      } else {
         StrTo=strdup(StrFrom);
      }
   }
   return(StrTo);
}

void strrep(char *Str,char Tok,char Rep) {

   if (Str) {
      while(*Str++!='\0')
      if (*Str==Tok)
         *Str=Rep;
   }
}

void strblank2end(char *Str,int Length) {
   int i;

   for (i=strlen(Str);i<Length; i++) {
      Str[i]=' ';
   }
   Str[Length-1] = '\0';
}

void strtrim(char *Str,char Tok) {

   register int i=0;
   char *s;

   if (Str && Str[0]!='\0') {
      /*Clear fisrt blanks*/
      while(*(Str+i)==Tok)
      i++;


      /*Copy chars, including \0, toward beginning to remove spaces*/
      s=Str;
      if (i) while(s<Str+strlen(Str)-i+1) { *s=*(s+i); s++; }

      /*Clear end blanks*/
      s=Str+strlen(Str);
      while(*--s==Tok)
         *s='\0';
   }
}

int strrindex(char *Str) {

   char *l,*r,*s;
   int   n=-1,k=0,t=1;

   if (Str) {
      s=strdup(Str);
      l=index(s,'(');
      r=index(s,')');

      if (!l || !r) {
         free(s);
         return(-1);
      }

      sscanf(l,"(%i)",&n);

      l=s;
      while(*s!='\0') {
         if (*s=='(') {
            t=0;
         }

         if (t) Str[k++]=*s;

         if (*s==')') {
            t=1;
         }
         s++;
      }
      Str[k]='\0';
      free(l);
   }
   return(n);
}

int strtok_count(char *Str,char Sep) {
 
   int n=0,s=1;
   
   while(*Str++!='\0') {
      if (*Str=='\n') break;
      
      if (*Str!=Sep) {
         if (s) {
            s=0;
            n++;
         }
      } else {
         s=1;
      }
   }
   return(n);
}

int strmatch(const char *Str,char *Pattern) {

   int     status=1;
   regex_t re;

   if (regcomp(&re,Pattern,REG_EXTENDED|REG_NOSUB|REG_ICASE)==0)  {
      status=regexec(&re,Str,(size_t)0,NULL,0);
      regfree(&re);
   }

   return(status);
}
