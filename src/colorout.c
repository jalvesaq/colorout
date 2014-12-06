/* This file is part of colorout R package
 **
 ** It is distributed under the GNU General Public License.
 ** See the file ../LICENSE for details.
 **
 ** Authors:
 ** (c) 2011-2014 Jakson Aquino: jalvesaq@gmail.com
 ** (c)      2014 Dominique-Laurent Couturier: dlc48@medschl.cam.ac.uk
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

static char crnormal[32], crnumber[32], crnegnum[32], crdate[32], crstring[32],
            crconst[32], crstderr[32], crwarn[32], crerror[32],
            crlogicalF[32], crlogicalT[32], crinfinite[32], crzero[32];
static int normalsize, numbersize, negnumsize, datesize, stringsize, constsize,
           logicalTsize, logicalFsize, infinitesize, zerosize;
static int colors_initialized = 0;
static int colorout_initialized = 0;

static double too_small = 1e-12;
static int hlzero = 0;

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


static int iszero(const char * b, int i, int len)
{
    char *charnum, *stopstring;
    double x;
    int j;
    j = i;
    charnum = (char*)calloc(sizeof(char),len);
    while(i<len){
        charnum[i-j] = b[i];
        i++;
    }
    x = strtod(charnum, &stopstring);
    free(charnum);
    if (x < too_small)
        return 1;
    else
        return 0;
}


static int isdate(const char * b, int i, int len)
{
    if((len-i)>9){
        /* YYYYxMMxDD or YYYYxDDxMM */
        if(b[i+4] == b[i+7] && (b[i+4]=='-'||b[i+4]=='/')){
            if(b[i]   >= '1' && b[i]   <= '9' &&
                    b[i+1] >= '0' && b[i+1] <= '9' &&
                    b[i+2] >= '0' && b[i+2] <= '9' &&
                    b[i+3] >= '0' && b[i+3] <= '9' &&
                    b[i+5] >= '0' && b[i+5] <= '3' &&
                    b[i+6] >= '0' && b[i+6] <= '9' &&
                    b[i+8] >= '0' && b[i+8] <= '3' &&
                    b[i+9] >= '0' && b[i+9] <= '9')
                return 1;
            else
                return 0;
            /* DDxMMxYYYY or MMxDDxYYYY  */
        } else if(b[i+2] == b[i+5] && (b[i+2]=='-'||b[i+2]=='/')){
            if(b[i]   >= '0' && b[i]   <= '3' &&
                    b[i+1] >= '0' && b[i+1] <= '9' &&
                    b[i+3] >= '0' && b[i+3] <= '3' &&
                    b[i+4] >= '0' && b[i+4] <= '9' &&
                    b[i+6] >= '1' && b[i+6] <= '9' &&
                    b[i+7] >= '0' && b[i+7] <= '9' &&
                    b[i+8] >= '0' && b[i+8] <= '9' &&
                    b[i+9] >= '0' && b[i+9] <= '9')
                return 1;
            else
                return 0;
        } else
            return 0;
        /* wrong length */
    } else
        return 0;
}


static int istime(const char * b, int i, int len)
{
    if((len-i)>7){
        /* HH:MM:SS */
        if(b[i+2] == ':' && b[i+5] == ':'){
            if(b[i  ] >= '0' && b[i]   <= '9' &&
                    b[i+1] >= '0' && b[i+1] <= '9' &&
                    b[i+3] >= '0' && b[i+3] <= '5' &&
                    b[i+4] >= '0' && b[i+4] <= '9' &&
                    b[i+6] >= '0' && b[i+6] <= '5' &&
                    b[i+7] >= '0' && b[i+7] <= '9')
                return 1;
            else
                return 0;
        } else
            return 0;
        /* wrong length */
    } else
        return 0;
}


void colorout_UnsetZero()
{
    hlzero = 0;
}

void colorout_SetZero(double *zr)
{
    hlzero = 1;
    too_small = *zr;
}


void colorout_SetColors(char **normal, char **number, char **negnum,
        char **datenum, char **string, char **constant, char **stderror,
        char **warn, char **error, char **logicalT, char **logicalF,
        char **infinite, char **zero, int *verbose, int *newline)
{
    strcpy(crnormal, normal[0]);
    strcpy(crnumber, number[0]);
    strcpy(crnegnum, negnum[0]);
    strcpy(crdate,   datenum[0]);
    strcpy(crstring, string[0]);
    strcpy(crconst,  constant[0]);
    strcpy(crstderr, stderror[0]);
    strcpy(crwarn,   warn[0]);
    strcpy(crerror,  error[0]);
    strcpy(crlogicalT, logicalT[0]);
    strcpy(crlogicalF, logicalF[0]);
    strcpy(crinfinite, infinite[0]);
    strcpy(crzero,     zero[0]);

    normalsize = strlen(crnormal);
    numbersize = strlen(crnumber);
    negnumsize = strlen(crnegnum);
    datesize = strlen(crdate);
    stringsize = strlen(crstring);
    constsize = strlen(crconst);
    logicalTsize = strlen(crlogicalT);
    logicalFsize = strlen(crlogicalF);
    infinitesize = strlen(crinfinite);
    zerosize     = strlen(crzero);

    if(*verbose){
        char buf1[64];
        char buf2[512];
        char buf3[512];
        sprintf(buf1, "%snormal\033[0m, ", crnormal);
        if(hlzero)
            sprintf(buf2, "%sx[x<=-%g]\033[0m, %sx[abs(x)<%g]\033[0m, %sx[x>=%g]\033[0m, %s19/01/2038 03:14:07\033[0m,",
                    crnegnum, too_small, crzero, too_small, crnumber, too_small, crdate);
        else
            sprintf(buf2, "%sx[x<0]\033[0m, %sx[x>=0]\033[0m, %s19/01/2038 03:14:07\033[0m,", crnegnum, crnumber, crdate);
        sprintf(buf3, "%s\"string\"\033[0m, %sNA/NaN/NULL\033[0m, %sFALSE\033[0m, %sTRUE\033[0m, %sInf\033[0m, %sstderror\033[0m, %swarn\033[0m, %serror\033[0m.\n",
                crstring, crconst, crlogicalF, crlogicalT, crinfinite, crstderr, crwarn, crerror);
        if(*newline)
            printf("%s%s\n%s", buf1, buf2, buf3);
        else
            printf("%s%s %s", buf1, buf2, buf3);
    }
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


/* this function color prints the centent of 'buf', of length 'len' and type 'otype' */
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
            hasWarn = strstr(buf, _("Lost warning messages"));
        if(hasWarn == NULL)
            hasWarn = strstr(buf, "Warning");
        if(hasWarn == NULL)
            hasWarn = strstr(buf, "WARNING");
        if(hasWarn == NULL)
            hasWarn = strstr(buf, "Lost warning messages");

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
        /* other type (i.e., non warning/error(s)). could be numbers, date, aso. */
    } else {
        l = len + 1024;
        newbuf = (char*)calloc(sizeof(char), l);
        l -= 64;
        strcpy(newbuf, crnormal);
        i = 0;
        j = normalsize;
        /* for all i smaller than obj length */
        while(i < len){
            if(j >= l)
                newbuf = colorout_make_bigger(newbuf, &l);
            /* NORMAL */
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
                /* NULL */
            } else if(bbuf[i] == 'N' && bbuf[i+1] == 'U' && bbuf[i+2] == 'L' && bbuf[i+3] == 'L'
                    && (bbuf[i+4] < 'A' || (bbuf[i+4] > 'Z' && bbuf[i+4] < 'a') || bbuf[i+4] > 'z')){
                sprintf(piece, "%sNULL%s", crconst, crnormal);
                strcat(newbuf, piece);
                i += 4;
                j += 4 + normalsize + constsize;
                /* TRUE */
            } else if(bbuf[i] == 'T' && bbuf[i+1] == 'R' && bbuf[i+2] == 'U' && bbuf[i+3] == 'E'
                    && (bbuf[i+4] < 'A' || (bbuf[i+4] > 'Z' && bbuf[i+4] < 'a') || bbuf[i+4] > 'z')){
                sprintf(piece, "%sTRUE%s", crlogicalT, crnormal);
                strcat(newbuf, piece);
                i += 4;
                j += 4 + normalsize + logicalTsize;
                /* FALSE */
            } else if(bbuf[i] == 'F' && bbuf[i+1] == 'A' && bbuf[i+2] == 'L' && bbuf[i+3] == 'S' && bbuf[i+4] == 'E'
                    && (bbuf[i+5] < 'A' || (bbuf[i+5] > 'Z' && bbuf[i+5] < 'a') || bbuf[i+5] > 'z')){
                sprintf(piece, "%sFALSE%s", crlogicalF, crnormal);
                strcat(newbuf, piece);
                i += 5;
                j += 5 + normalsize + logicalFsize;
                /* NA */
            } else if(bbuf[i] == 'N' && bbuf[i+1] == 'A'
                    && (bbuf[i+2] < 'A' || (bbuf[i+2] > 'Z' && bbuf[i+2] < 'a') || bbuf[i+2] > 'z')){
                sprintf(piece, "%sNA%s", crconst, crnormal);
                strcat(newbuf, piece);
                i += 2;
                j += 2 + normalsize + constsize;
                /* Inf */
            } else if(bbuf[i] == 'I' && bbuf[i+1] == 'n' && bbuf[i+2] == 'f'
                    && (bbuf[i+3] < 'A' || (bbuf[i+3] > 'Z' && bbuf[i+3] < 'a') || bbuf[i+3] > 'z')){
                if(i > 0 && bbuf[i-1] == '-'){
                    newbuf[j-1] = 0;
                    sprintf(piece, "%s-Inf%s", crinfinite, crnormal);
                    strcat(newbuf, piece);
                } else {
                    sprintf(piece, "%sInf%s", crinfinite, crnormal);
                    strcat(newbuf, piece);
                }
                i += 3;
                j += 3 + normalsize + infinitesize;
                /* NaN */
            } else if(bbuf[i] == 'N' && bbuf[i+1] == 'a' && bbuf[i+2] == 'N'
                    && (bbuf[i+3] < 'A' || (bbuf[i+3] > 'Z' && bbuf[i+3] < 'a') || bbuf[i+3] > 'z')){
                sprintf(piece, "%sNaN%s", crconst, crnormal);
                strcat(newbuf, piece);
                i += 3;
                j += 3 + normalsize + constsize;
                /* hexadecimal number */
            } else if(bbuf[i] == '0' && bbuf[i+1] == 'x' && ((bbuf[i+2] >= '0' && bbuf[i+2] <= '9') ||
                        (bbuf[i+2] >= 'a' && bbuf[i+2] <= 'f'))){
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
                /* date YYYYxMMxDD or DDxMMxYYYY */
            } else if(isdate(bbuf, i, len)){
                strcat(newbuf, crdate);
                j += datesize;
                for(int k = 0; k < 10; k++){
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                }
                /* if time is appended to the date */
                if(bbuf[i] == ' ' && istime(bbuf, i+1, len)){
                    for(int k = 0; k < 9; k++){
                        newbuf[j] = bbuf[i];
                        i++;
                        j++;
                    }
                }
                strcat(newbuf, crnormal);
                j += normalsize;
                /* time */
            } else if(istime(bbuf, i, len)){
                strcat(newbuf, crdate);
                j += datesize;
                for(int k = 0; k < 8; k++){
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                }
                strcat(newbuf, crnormal);
                j += normalsize;
                /* positive numbers */
            } else if(bbuf[i] >= '0' && bbuf[i] <= '9' && isnumber(bbuf, i, len)){
                if (hlzero && iszero(bbuf, i, len)){
                    strcat(newbuf, crzero);
                    j += zerosize;
                }else{
                    strcat(newbuf, crnumber);
                    j += numbersize;
                }
                while(i < len && ((bbuf[i] >= '0' && bbuf[i] <= '9') || bbuf[i] == '.')){
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
                /* negative numbers */
            } else if(bbuf[i] == '-' && (bbuf[i+1] >= '0' && bbuf[i+1] <= '9') && isnumber(bbuf, i+1, len)){
                if (hlzero && iszero(bbuf, i+1, len)){
                    strcat(newbuf, crzero);
                    j += zerosize;
                }else{
                    strcat(newbuf, crnegnum);
                    j += negnumsize;
                }
                newbuf[j] = '-';
                i++;
                j++;
                while(i < len && ((bbuf[i] >= '0' && bbuf[i] <= '9') || bbuf[i] == '.')){
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
                /* 'words' */
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
                /* anything else */
            } else {
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
        if(strcmp(getenv("TERM"), "fbterm") == 0){
            strcpy(crnormal, "\033[1;40}");
            strcpy(crnumber, "\033[1;214}");
            strcpy(crnegnum, "\033[1;209}");
            strcpy(crdate,   "\033[1;179}");
            strcpy(crstring, "\033[1;85}");
            strcpy(crconst,  "\033[1;35}");
            strcpy(crstderr, "\033[1;33}");
            strcpy(crwarn,   "\033[1;1}");
            strcpy(crerror,  "\033[2;1}\033[1;7}");
            strcpy(crlogicalT, "\033[1;78}");
            strcpy(crlogicalF, "\033[1;203}");
            strcpy(crinfinite, "\033[1;39}");
            strcpy(crzero,     "\033[1;226}");
        } else {
            if(strstr(getenv("TERM"), "256")){
                strcpy(crnormal, "\033[0;38;5;40m");
                strcpy(crnumber, "\033[0;38;5;214m");
                strcpy(crnegnum, "\033[0;38;5;209m");
                strcpy(crdate,   "\033[0;38;5;179m");
                strcpy(crstring, "\033[0;38;5;85m");
                strcpy(crconst,  "\033[0;38;5;35m");
                strcpy(crstderr, "\033[0;38;5;33m");
                strcpy(crwarn,   "\033[0;1;38;5;1m");
                strcpy(crerror,  "\033[0;48;5;1;38;5;15m");
                strcpy(crlogicalT, "\033[0;38;5;78m");
                strcpy(crlogicalF, "\033[0;38;5;203m");
                strcpy(crinfinite, "\033[0;38;5;39m");
                strcpy(crzero,     "\033[0;38;5;226m");
            } else {
                strcpy(crnormal, "\033[32m");
                strcpy(crnumber, "\033[33m");
                strcpy(crnegnum, "\033[33m");
                strcpy(crdate,   "\033[33m");
                strcpy(crstring, "\033[36m");
                strcpy(crconst,  "\033[35m");
                strcpy(crstderr, "\033[34m");
                strcpy(crwarn,   "\033[1;31m");
                strcpy(crerror,  "\033[41;37m");
                strcpy(crlogicalT, "\033[35m");
                strcpy(crlogicalF, "\033[35m");
                strcpy(crinfinite, "\033[35m");
                strcpy(crzero,     "\033[33m");
            }
        }
        normalsize = strlen(crnormal);
        numbersize = strlen(crnumber);
        negnumsize = strlen(crnegnum);
        datesize = strlen(crdate);
        stringsize = strlen(crstring);
        constsize = strlen(crconst);
        logicalTsize = strlen(crlogicalT);
        logicalFsize = strlen(crlogicalF);
        infinitesize = strlen(crinfinite);
        zerosize     = strlen(crzero);
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
