/* This file is part of colorout R package
**
** It is distributed under the GNU General Public License.
** See the file ../LICENSE for details.
** 
** (c) 2011 Jakson Aquino: jalvesaq@gmail.com
**
***************************************************************/


#include <R.h>  /* to include Rconfig.h */

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("colorout", String)
#else
#define _(String) (String)
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <Rinterface.h>

#define R_INTERFACE_PTRS 1

/* We could use "extern char OutDec;" and test only OutDec instead of always
   testing both '.' and ',', but some functions (like mtable of memisc
   package) always print a '.' regardless of the value of OutDec, and ',' is
   the thousands separator in the US and GB. */

extern void (*ptr_R_WriteConsole)(const char *, int);
extern void (*ptr_R_WriteConsoleEx)(const char *, int, int);

static void (*save_ptr_R_WriteConsole)(const char *, int);
static void (*save_ptr_R_WriteConsoleEx)(const char *, int, int);
static void *save_R_Outputfile;
static void *save_R_Consolefile;

static char crnormal[32], crnumber[32], crnegnum[32], crstring[32],
     crconst[32], crstderr[32], crwarn[32], crerror[32];
static int normalsize, numbersize, negnumsize, stringsize, constsize;
static int colors_initialized = 0;
static int colorout_initialized = 0;

static int isnumber(const char * b, int i, int len)
{
    int l = len;
    if(l > (i + 5))
        l = i + 5;
    i++;
    while(i < l){
        if(((b[i] > 0   && b[i] < '0') || (b[i] > '9' && b[i] < 'A') || (b[i] > 'Z' && b[i] < 'a') || (b[i] > 'z' && b[i] > 0)) &&
                b[i] != '.' && b[i] != ',' &&
                !(b[i] == 'e' && (i + 2) < len && (b[i+1] == '-' || b[i+1] == '+') && b[i+2] >= '0' && b[i+2] <= '9') &&
                !(b[i-1] == 'e' && (b[i] == '-' || b[i] == '+')))
            break;
        if((b[i] < '0' || b[i] > '9') &&
                b[i] != '.' && b[i] != ',' &&
                !(b[i] == 'e' && (i + 2) < len && (b[i+1] == '-' || b[i+1] == '+') && b[i+2] >= '0' && b[i+2] <= '9') &&
                !(b[i-1] == 'e' && (b[i] == '-' || b[i] == '+')))
            return 0;
        i++;
    }
    return 1;
}

void colorout_SetColors(char **normal, char **number, char **negnum, char **string,
        char **constant, char **stderror, char **warn, char **error,
        int *verbose)
{
    strcpy(crnormal, normal[0]);
    strcpy(crnumber, number[0]);
    strcpy(crnegnum, negnum[0]);
    strcpy(crstring, string[0]);
    strcpy(crconst,  constant[0]);
    strcpy(crstderr, stderror[0]);
    strcpy(crwarn,   warn[0]);
    strcpy(crerror,  error[0]);

    normalsize = strlen(crnormal);
    numbersize = strlen(crnumber);
    negnumsize = strlen(crnegnum);
    stringsize = strlen(crstring);
    constsize = strlen(crconst);

    if(*verbose)
      printf("%snormal\033[0m, %snumber\033[0m, %snegnum\033[0m, %sstring\033[0m, %sconst\033[0m, %sstderror\033[0m, %swarn\033[0m, %serror\033[0m.\n",
          crnormal, crnumber, crnegnum, crstring, crconst, crstderr, crwarn, crerror);
}

char *colorout_make_bigger(char *ptr, int *len)
{
    char *nnbuf;
    *len = *len + 1024;
    nnbuf = (char*)calloc(1, *len);
    strcpy(nnbuf, ptr);
    free(ptr);
    *len = *len - 64;
    return(nnbuf);
}

void colorout_R_WriteConsoleEx (const char *buf, int len, int otype)
{
    char *newbuf, *bbuf;
    char piece[64];
    int neednl, i, j, l;

    /* gnome-terminal extends the background color for the other line
     * if the "\033[0m" is after the newline*/
    neednl = 0;
    bbuf = (char*)malloc((len+1) * sizeof(char));
    strcpy(bbuf, buf);
    if(buf[len - 1] == '\n'){
        neednl = 1;
        bbuf[len - 1] = 0;
        len -= 1;
    }

    /* It would be better if 'otype' was passed with more than two possible
     * values (0 and 1). It would be nice if it had at least four values:
     * 0 = Normal output (should be sent to stdout)
     * 1 = Information
     * 2 = Warning
     * 3 = Error
     * */
    if(otype){
        /* The value of otype does not tell whether the message is
         * Information, Warning or Error. So, we have to guess: */
        char *hasWarn = strstr(buf, _("Warning"));
        if(hasWarn == NULL)
            hasWarn = strstr(buf, _("WARNING"));
        if(hasWarn == NULL)
            hasWarn = strstr(buf, "Warning");
        if(hasWarn == NULL)
            hasWarn = strstr(buf, "WARNING");

        char *hasError = strstr(buf, _("Error"));
        if(hasError == NULL)
            hasError = strstr(buf, _("ERROR"));
        if(hasError == NULL)
            hasError = strstr(buf, "Error");
        if(hasError == NULL)
            hasError = strstr(buf, "ERROR");

        /* Print the message */
        if(hasWarn == buf)
            fprintf(stderr, "%s%s\033[0m", crwarn, bbuf);
        else if(hasError == buf)
            fprintf(stderr, "%s%s\033[0m", crerror, bbuf);
        else
            fprintf(stderr, "%s%s\033[0m", crstderr, bbuf);

        /* Put the newline back */
        if(neednl)
            fprintf(stderr, "\n");

        fflush(stderr);
    } else {
        l = len + 1024;
        newbuf = (char*)calloc(sizeof(char), l);
        l -= 64;
        strcpy(newbuf, crnormal);
        i = 0;
        j = normalsize;
        while(i < len){
            if(j >= l)
                newbuf = colorout_make_bigger(newbuf, &l);
            if(bbuf[i] == '"'){
                strcat(newbuf, crstring);
                j += stringsize;
                newbuf[j] = bbuf[i];
                i++;
                j++;
                while(i < len && bbuf[i] != '\n'){
                    if(j >= l)
                        newbuf = colorout_make_bigger(newbuf, &l);
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                    if(i > 2 && bbuf[i-1] == '"' && bbuf[i-2] != '\\')
                        break;
                }
                strcat(newbuf, crnormal);
                j += normalsize;
            } else if(bbuf[i] == 'N' && bbuf[i+1] == 'U' && bbuf[i+2] == 'L' && bbuf[i+3] == 'L'
                    && (bbuf[i+4] < 'A' || (bbuf[i+4] > 'Z' && bbuf[i+4] < 'a') || bbuf[i+4] > 'z')){
                sprintf(piece, "%sNULL%s", crconst, crnormal);
                strcat(newbuf, piece);
                i += 4;
                j += 4 + normalsize + constsize;
            } else if(bbuf[i] == 'T' && bbuf[i+1] == 'R' && bbuf[i+2] == 'U' && bbuf[i+3] == 'E'
                    && (bbuf[i+4] < 'A' || (bbuf[i+4] > 'Z' && bbuf[i+4] < 'a') || bbuf[i+4] > 'z')){
                sprintf(piece, "%sTRUE%s", crconst, crnormal);
                strcat(newbuf, piece);
                i += 4;
                j += 4 + normalsize + constsize;
            } else if(bbuf[i] == 'F' && bbuf[i+1] == 'A' && bbuf[i+2] == 'L' && bbuf[i+3] == 'S' && bbuf[i+4] == 'E'
                    && (bbuf[i+5] < 'A' || (bbuf[i+5] > 'Z' && bbuf[i+5] < 'a') || bbuf[i+5] > 'z')){
                sprintf(piece, "%sFALSE%s", crconst, crnormal);
                strcat(newbuf, piece);
                i += 5;
                j += 5 + normalsize + constsize;
            } else if(bbuf[i] == 'N' && bbuf[i+1] == 'A'
                    && (bbuf[i+2] < 'A' || (bbuf[i+2] > 'Z' && bbuf[i+2] < 'a') || bbuf[i+2] > 'z')){
                sprintf(piece, "%sNA%s", crconst, crnormal);
                strcat(newbuf, piece);
                i += 2;
                j += 2 + normalsize + constsize;
            } else if(bbuf[i] == 'I' && bbuf[i+1] == 'n' && bbuf[i+2] == 'f'
                    && (bbuf[i+3] < 'A' || (bbuf[i+3] > 'Z' && bbuf[i+3] < 'a') || bbuf[i+3] > 'z')){
                if(i > 0 && bbuf[i-1] == '-'){
                    newbuf[j-1] = 0;
                    sprintf(piece, "%s-Inf%s", crconst, crnormal);
                    strcat(newbuf, piece);
                } else {
                    sprintf(piece, "%sInf%s", crconst, crnormal);
                    strcat(newbuf, piece);
                }
                i += 3;
                j += 3 + normalsize + constsize;
            } else if(bbuf[i] == 'N' && bbuf[i+1] == 'a' && bbuf[i+2] == 'N'
                    && (bbuf[i+3] < 'A' || (bbuf[i+3] > 'Z' && bbuf[i+3] < 'a') || bbuf[i+3] > 'z')){
                sprintf(piece, "%sNaN%s", crconst, crnormal);
                strcat(newbuf, piece);
                i += 3;
                j += 3 + normalsize + constsize;
            } else if(bbuf[i] == '0' && bbuf[i+1] == 'x' && ((bbuf[i+2] >= '0' && bbuf[i+2] <= '9') ||
                            (bbuf[i+2] >= 'a' && bbuf[i+2] <= 'f'))){ /* hexadecimal */
                strcat(newbuf, crnumber);
                j += numbersize;
                newbuf[j] = bbuf[i];
                newbuf[j+1] = 'x';
                i += 2;
                j += 2;
                while(i < len && ((bbuf[i] >= '0' && bbuf[i] <= '9') || (bbuf[i] >= 'a' && bbuf[i] <= 'f'))){
                    if(j >= l)
                        newbuf = colorout_make_bigger(newbuf, &l);
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                }
                strcat(newbuf, crnormal);
                j += normalsize;
            } else if(bbuf[i] == '-' && (bbuf[i+1] >= '0' && bbuf[i+1] <= '9')){
                strcat(newbuf, crnegnum); /* negative numbers */
                j += negnumsize;
                newbuf[j] = '-';
                i++;
                j++;
                while(i < len && ((bbuf[i] >= '0' && bbuf[i] <= '9') || bbuf[i] == '.' || bbuf[i] == ',')){
                    if(j >= l)
                        newbuf = colorout_make_bigger(newbuf, &l);
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                    if(bbuf[i] == 'e' && (i + 2) < len && (bbuf[i+1] == '-' || bbuf[i+1] == '+') && (bbuf[i+2] >= '0' && bbuf[i+2] <= '9')){
                        newbuf[j] = bbuf[i];
                        i++;
                        j++;
                        newbuf[j] = bbuf[i];
                        i++;
                        j++;
                    }
                }
                strcat(newbuf, crnormal);
                j += normalsize;
            } else if(bbuf[i] >= '0' && bbuf[i] <= '9' && isnumber(bbuf, i, len)){
                strcat(newbuf, crnumber); /* positive numbers */
                j += numbersize;
                while(i < len && ((bbuf[i] >= '0' && bbuf[i] <= '9') || bbuf[i] == '.' || bbuf[i] == ',')){
                    if(j >= l)
                        newbuf = colorout_make_bigger(newbuf, &l);
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                    if(bbuf[i] == 'e' && (i + 2) < len && (bbuf[i+1] == '-' || bbuf[i+1] == '+') && (bbuf[i+2] >= '0' && bbuf[i+2] <= '9')){
                        newbuf[j] = bbuf[i];
                        i++;
                        j++;
                        newbuf[j] = bbuf[i];
                        i++;
                        j++;
                    }
                }
                strcat(newbuf, crnormal);
                j += normalsize;
            } else if(i < len && ((bbuf[i] >= 'A' && bbuf[i] <= 'Z')
                        || (bbuf[i] >= 'a' && bbuf[i] <= 'z') || bbuf[i] == '.' || bbuf[i] == '_' || (signed char)bbuf[i] < 0)){
                /* Normal word: get it as a whole */
                while(i < len && ((bbuf[i] >= '0' && bbuf[i] <= '9') || (bbuf[i] >= 'A' && bbuf[i] <= 'Z') ||
                            (bbuf[i] >= 'a' && bbuf[i] <= 'z') || bbuf[i] == '.' || bbuf[i] == '_' || (signed char)bbuf[i] < 0)){
                    if(j >= l)
                        newbuf = colorout_make_bigger(newbuf, &l);
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                }
            } else { /* anything else */
                newbuf[j] = bbuf[i];
                i++;
                j++;
            }
        }


        if(neednl)
            printf("%s\033[0m\n", newbuf);
        else
            printf("%s\033[0m", newbuf);

        free(newbuf);
        fflush(stdout);
    }
    free(bbuf);
}

void colorout_ColorOutput()
{
    if(colorout_initialized)
        return;

    if(colors_initialized == 0){
        strcpy(crnormal, "\033[32m");
        strcpy(crnumber, "\033[33m");
        strcpy(crnegnum, "\033[33m");
        strcpy(crstring, "\033[36m");
        strcpy(crconst,  "\033[35m");
        strcpy(crstderr, "\033[34m");
        strcpy(crwarn,  "\033[1;31m");
        strcpy(crerror,  "\033[41;37m");
        normalsize = 5;
        numbersize = 5;
        negnumsize = 5;
        stringsize = 5;
        constsize  = 5;
        colors_initialized = 1;
    }

    /* Replace Rstd_WriteConsoleEx() with colorout_R_WriteConsoleEx().
     * See R source code: files src/unix/system.c and src/unix/sys-std.c */
    save_R_Outputfile = R_Outputfile;
    save_R_Consolefile = R_Consolefile;
    save_ptr_R_WriteConsole = ptr_R_WriteConsole;
    save_ptr_R_WriteConsoleEx = ptr_R_WriteConsoleEx;

    R_Outputfile = NULL;
    R_Consolefile = NULL;
    ptr_R_WriteConsole = NULL;
    ptr_R_WriteConsoleEx = colorout_R_WriteConsoleEx;

    colorout_initialized = 1;
}

void colorout_noColorOutput()
{
    if(colorout_initialized){
        R_Outputfile = save_R_Outputfile;
        R_Consolefile = save_R_Consolefile;
        ptr_R_WriteConsole = save_ptr_R_WriteConsole;
        ptr_R_WriteConsoleEx = save_ptr_R_WriteConsoleEx;
        colorout_initialized = 0;
    }
}
