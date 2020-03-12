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
#include <Rinternals.h>

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

static char crnormal[64], crnumber[64], crnegnum[64], crdate[64], crstring[64],
            crconst[64], crstderr[64], crwarn[64], crerror[64],
            crlogicalF[64], crlogicalT[64], crinfinite[64], crindex[64], crzero[64];
static int normalsize, numbersize, negnumsize, datesize, stringsize, constsize,
           logicalTsize, logicalFsize, infinitesize, indexsize, zerosize;
static int colors_initialized = 0;
static int colorout_initialized = 0;
static char *piece;

static double too_small = 1e-12;
static int hlzero = 0;

typedef struct pattern {
    char *ptrn;
    char *compiled;
    int cpldsize;
    int matchsize;
    char *color;
    int crsize;
    struct pattern * next;
} pattern_t;

pattern_t *P = NULL;

static int isnumber(const char * b, int i, int len)
{
    int l = len;
    if(l > (i + 5))
        l = i + 5;
    i++;
    while(i < l){
        if(((b[i] > 0 && b[i] < '0') || (b[i] > '9' && b[i] < 'A') || (b[i] > 'Z' && b[i] < 'a') || (b[i] > 'z' && b[i] > 0)) &&
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
    charnum = (char*)calloc(sizeof(char),len+1);
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

static int isindex(const char * b, int i, int len)
{
    // Element of unnamed list
    if(i > 1 && b[i-2] == '[')
        return 0;

    if(b[i] == ','){
        i++;
        if(!((b[i] >= '0' && b[i] <= '9') || b[i] == ' '))
            return 0;
    }
    while(i < len && ((b[i] >= '0' && b[i] <= '9') || b[i] == ' '))
        i++;

    // Vector or matrix index?
    // Also check that the previous index is a digit
    if ((b[i] == ']' && b[i-1] >= '0' && b[i-1] <= '9') || (b[i] == ',' && b[i+1] == ']' && b[i-1] >= '0' && b[i-1] <= '9'))
        return 1;
    else
        return 0;
}



static int isdate(const char * b, int i, int len)
{
    if((len-i)>9){
        /* YYYYxMMxDD or YYYYxDDxMM */
        if(b[i+4] == b[i+7] && b[i+4] == '-'){
            if(b[i] >= '1' && b[i] <= '9' &&
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
            /* DDxMMxYYYY or MMxDDxYYYY */
        } else if(b[i+2] == b[i+5] && b[i+2] == '-'){
            if(b[i] >= '0' && b[i] <= '3' &&
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
            if(b[i] >= '0' && b[i] <= '9' &&
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

static int ispattern(const char * b, int i, int len, const pattern_t *p)
{
    int n = 0;
    int j = i;
    if((len-i) >= p->matchsize){
        while(n < p->cpldsize){
            if(p->compiled[n] == b[j]){
                n++;
                j++;
            } else if(p->compiled[n] == '\x02' && b[j] >= p->compiled[n+1] && b[j] <= p->compiled[n+2]){
                n += 3;
                j++;
            } else if(p->compiled[n] == '\x03'){
                n++;
                if(p->compiled[n] == '\x02' && b[j] >= p->compiled[n+1] && b[j] <= p->compiled[n+2]){
                    while(b[j] >= p->compiled[n+1] && b[j] <= p->compiled[n+2])
                        j++;
                    n += 3;
                } else {
                    while(p->compiled[n] == b[j])
                        j++;
                    n++;
                }
            } else {
                break;
            }
        }
    }
    if(n == p->cpldsize && (j - i) >= p->matchsize)
        return (j - i);
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

SEXP colorout_ListPatterns()
{
    int n = 0;
    pattern_t *p = P;

    while(p){
        n++;
        /*
        printf("pattern = %s\nmatch size = %d\ncompiled = ", p->ptrn, p->matchsize);
        for(int i = 0; i < strlen(p->compiled); i++)
            if(p->compiled[i] == '\x02')
                printf("%s2\033[0m", crnumber);
            else if (p->compiled[i] == '\x03')
                printf("%s3\033[0m", crnumber);
            else
                printf("%c", p->compiled[i]);
        printf("\ncompiled size = %d\n", p->cpldsize);
        */
        p = p->next;
    }

    SEXP res = PROTECT(allocVector(STRSXP, n));
    SEXP nms = PROTECT(allocVector(STRSXP, n));

    p = P;
    int i = 0;
    while(p){
        SET_STRING_ELT(nms, i, mkChar(p->ptrn));
        SET_STRING_ELT(res, i, mkChar(p->color));
        p = p->next;
        i++;
    }
    setAttrib(res, R_NamesSymbol, nms);
    UNPROTECT(2);
    return res;
}

void colorout_DeletePattern(char **pattern)
{
    pattern_t *p = P;
    pattern_t *prev = NULL;
    pattern_t *next = NULL;
    while(p){
        next = p->next;
        if(strcmp(*pattern, p->ptrn) == 0){
            if(prev)
                prev->next = p->next;
            free(p->ptrn);
            free(p->compiled);
            free(p->color);
            free(p);
            if(p == P)
                P = next;
        } else {
            prev = p;
        }
        p = next;
    }
}

void colorout_AddPattern(char **pattern, char **color)
{
    pattern_t *p = (pattern_t*)calloc(1, sizeof(pattern_t));
    p->ptrn = (char*)malloc(strlen(*pattern)+1);
    strcpy(p->ptrn, *pattern);

    // Compile the pattern for faster comparison
    p->compiled = (char*)calloc(1, strlen(*pattern)+1);
    int i = 0;
    int j = 0;
    int l = strlen(p->ptrn);
    p->matchsize = 0;
    while(i < l){
        if(i > 0 && p->ptrn[i] == '*' && p->ptrn[i-1] != '\\'){
            // Put x03 before the pattern to be repeated
            if(i > 4 && p->ptrn[i-5] == '[' && p->ptrn[i-3] == '-' && p->ptrn[i-1] == ']'){
                p->compiled[j] = p->compiled[j-1];
                p->compiled[j-1] = p->compiled[j-2];
                p->compiled[j-2] = p->compiled[j-3];
                p->compiled[j-3] = '\x03';
                j++;
                i++;
            } else {
                p->compiled[j] = p->compiled[j-1];
                p->compiled[j-1] = '\x03';
                j++;
                i++;
            }
        } else if(i < (l - 4) && p->ptrn[i] == '[' && p->ptrn[i + 2] == '-' && p->ptrn[i + 4] == ']'){
            p->compiled[j] = '\x02';
            p->compiled[j+1] = p->ptrn[i+1];
            p->compiled[j+2] = p->ptrn[i+3];
            i += 5;
            j += 3;
            p->matchsize++;
        } else {
            p->compiled[j] = p->ptrn[i];
            i++;
            j++;
            p->matchsize++;
        }
    }
    p->cpldsize = strlen(p->compiled);

    p->color = (char*)malloc(strlen(*color)+1);
    strcpy(p->color, *color);
    p->crsize = strlen(p->color);

    if(P)
        p->next = P;

    P = p;
}

static int max(const int a, const int b)
{
    if(a > b)
        return a;
    return b;
}

static void alloc_piece()
{
    int maxsize;

    maxsize = max(logicalTsize, logicalFsize);
    maxsize = max(maxsize, constsize);
    maxsize = max(maxsize, infinitesize);

    if(piece != NULL)
        free(piece);
    piece = (char*)calloc(1, maxsize + 6 + normalsize);
}

void colorout_SetColors(char **normal, char **number, char **negnum,
        char **datenum, char **string, char **constant, char **stderror,
        char **warn, char **error, char **logicalT, char **logicalF,
        char **infinite, char **index, char **zero, int *verbose)
{
    strncpy(crnormal, normal[0], 63);
    strncpy(crnumber, number[0], 63);
    strncpy(crnegnum, negnum[0], 63);
    strncpy(crdate,   datenum[0], 63);
    strncpy(crstring, string[0], 63);
    strncpy(crconst,  constant[0], 63);
    strncpy(crstderr, stderror[0], 63);
    strncpy(crwarn,   warn[0], 63);
    strncpy(crerror,  error[0], 63);
    strncpy(crlogicalT, logicalT[0], 63);
    strncpy(crlogicalF, logicalF[0], 63);
    strncpy(crinfinite, infinite[0], 63);
    strncpy(crindex, index[0], 63);
    strncpy(crzero,     zero[0], 63);

    normalsize = strlen(crnormal);
    numbersize = strlen(crnumber);
    negnumsize = strlen(crnegnum);
    datesize = strlen(crdate);
    stringsize = strlen(crstring);
    constsize = strlen(crconst);
    logicalTsize = strlen(crlogicalT);
    logicalFsize = strlen(crlogicalF);
    infinitesize = strlen(crinfinite);
    indexsize = strlen(crindex);
    zerosize     = strlen(crzero);

    alloc_piece();

    if(*verbose){
        printf("%snormal\033[0m ", crnormal);
        if(hlzero)
            printf("%sx[x<=-%g]\033[0m %sx[abs(x)<%g]\033[0m %sx[x>=%g]\033[0m ",
                    crnegnum, too_small, crzero, too_small, crnumber, too_small);
        else
            printf("%sx[x<0]\033[0m %sx[x>=0]\033[0m ", crnegnum, crnumber);

        printf("%s19/01/2038 03:14:07\033[0m %s\"string\"\033[0m\n%sNA/NaN/NULL\033[0m %sFALSE\033[0m %sTRUE\033[0m %sInf\033[0m %s[index]\033[0m %sstderror\033[0m %swarn\033[0m %serror\033[0m\n",
                crdate, crstring, crconst, crlogicalF, crlogicalT, crinfinite, crindex, crstderr, crwarn, crerror);
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


/* This function color prints the contents of 'buf', of length 'len' and type 'otype' */
void colorout_R_WriteConsoleEx (const char *buf, int len, int otype)
{
    char *newbuf, *bbuf;
    int neednl, i, j, l;

    /* Do nothing if the output was already colorized by another package */
    for(i = 0; i < len; i++)
        if(buf[i] == 0x1b){
            printf("%s", buf);
            return;
        }

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
        int haspttrn;
        while(i < len){
            if(j >= l)
                newbuf = colorout_make_bigger(newbuf, &l);
            /* Custom patterns */
            haspttrn = 0;
            if(P){
                int psz = 0;
                pattern_t *p = P;
                while(p){
                    psz = ispattern(bbuf, i, len, p);
                    if(psz){
                        haspttrn = 1;
                        strcat(newbuf, p->color);
                        j += p->crsize;
                        for(int k = 0; k < psz; k++){
                            newbuf[j] = bbuf[i];
                            i++;
                            j++;
                        }
                        strcat(newbuf, crnormal);
                        j += normalsize;
                    }
                    p = p->next;
                }
                if(haspttrn)
                    continue;
            }
            if(haspttrn)
                continue;
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
            } else if(bbuf[i] == '[' && ((bbuf[i+1] >= '0' && bbuf[i+1] <= '9') || bbuf[i+1] == ',' || bbuf[i+1] == ' ') && isindex(bbuf, i+1, len)){
                strcat(newbuf, crindex);
                j += indexsize;
                newbuf[j] = bbuf[i];
                i++;
                j++;
                while(bbuf[i] != ']'){
                    if(j > l)
                        newbuf = colorout_make_bigger(newbuf, &l);
                    newbuf[j] = bbuf[i];
                    i++;
                    j++;
                }
                newbuf[j] = bbuf[i];
                i++;
                strcat(newbuf, crnormal);
                j += normalsize + 1;
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
            strcpy(crstderr, "\033[1;213}");
            strcpy(crwarn,   "\033[1;1}");
            strcpy(crerror,  "\033[2;1}\033[1;7}");
            strcpy(crlogicalT, "\033[1;78}");
            strcpy(crlogicalF, "\033[1;203}");
            strcpy(crinfinite, "\033[1;39}");
            strcpy(crindex, "\033[1;30}");
            strcpy(crzero,     "\033[1;226}");
        } else {
            strcpy(crnormal, "\033[0;38;5;40m");
            strcpy(crnumber, "\033[0;38;5;214m");
            strcpy(crnegnum, "\033[0;38;5;209m");
            strcpy(crdate,   "\033[0;38;5;179m");
            strcpy(crstring, "\033[0;38;5;85m");
            strcpy(crconst,  "\033[0;38;5;35m");
            strcpy(crstderr, "\033[0;38;5;213m");
            strcpy(crwarn,   "\033[0;1;38;5;1m");
            strcpy(crerror,  "\033[0;48;5;1;38;5;15m");
            strcpy(crlogicalT, "\033[0;38;5;78m");
            strcpy(crlogicalF, "\033[0;38;5;203m");
            strcpy(crinfinite, "\033[0;38;5;39m");
            strcpy(crindex, "\033[0;38;5;30m");
            strcpy(crzero,     "\033[0;38;5;226m");
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
        indexsize = strlen(crindex);
        zerosize     = strlen(crzero);
        colors_initialized = 1;

        alloc_piece();
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


SEXP colorout_is_enabled() {
  return ScalarLogical(colorout_initialized);
}
