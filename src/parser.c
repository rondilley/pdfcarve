/*****
 *
 * Description: PDF Parser functions
 * 
 * Copyright (c) 2008-2016, Ron Dilley
 * All rights reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 ****/

/****
 *
 * defines
 *
 ****/

#define PDF_FILTER_NONE 0
#define PDF_FILTER_ZLIB 1

#define MAX_OBJ_DEPTH 64

/****
 *
 * includes
 *
 ****/

#include "parser.h"

/****
 *
 * local variables
 *
 ****/

/****
 *
 * external global variables
 *
 ****/

extern Config_t *config;

/****
 *
 * global variables
 *
 ****/

enum { PDF_TYPE_NONE,
       PDF_TYPE_REALNUM, /* 343 = realNum */
       PDF_TYPE_INTNUM, /* 3.4 = intNum */
       PDF_TYPE_BOOL, /* true | false = bool */
       PDF_TYPE_EOL, /* \n\r | \n | \n\r\n\r = EOL */
       PDF_TYPE_COMMENT, /* % ... EOL = comment */
       PDF_TYPE_NAME, /* / ... = name */
       PDF_TYPE_STRING, /* ( ... ) = literalString */
       PDF_TYPE_UTFSTRING, /* ( 254 255 ... ) = utf encoded literal string */
       PDF_TYPE_DATESTRING, /* (D:YYYYMMDDHHmmSSOHH'mm') = date encoded literal string */
       PDF_TYPE_HEXSTRING, /* < ... > = hexString */
       PDF_TYPE_UTFHEXSTRING, /* < fe ff ... > = utf encoded hex string */
       PDF_TYPE_ARRAY, /* [ ... ] = array NOTE can be an array of any n objects */
                       /* << /Kids [ 1 0 R 1 0 R ]  = name tree */
                       /*    /Names [ (key1) 1 0 R (key2) 2 0 R ] */
                       /*    /Limits [ (firstKey) (lastKey) ] */
       PDF_TYPE_DICTIONARY, /* << name ... >> = dictionary where key is a name and value can be any object */
       PDF_TYPE_OBJECT, /* 3 0 obj ... endobj = turns any object into an indirect (referencable) object */
                        /*                      3 = objNum, 0 = objRev */
       PDF_TYPE_STREAM, /* << ... >>EOLstream\n\r or \r ... \n\r endstream = stream */
       PDF_TYPE_HEADER, /* %PDF-1.6EOL%nnnnEOL = file header where each n is > 128 to make file look binary */
       PDF_TYPE_FOOTER, /* xref ... trailer ... startxref ... %%EOF = document footer */
                        /* 3 0 R = reference to indirect object 3 0 (it's a pointer) */
                        /* ' ' | '  ' | '\t' | '\t\t' = ' ' outside of ( ... ) */
       PDF_TYPE_REFERENCE,
       PDF_TYPE_EOF,
};

char *typeStrings[] = { "NONE",
			"Real Number",
			"Integer",
			"Boolean",
			"EOL",
			"Comment",
			"Name",
			"String",
			"UTF String",
			"Date String",
			"HEX String",
			"UTF HEX String",
			"Array",
			"Dictionary",
			"Object",
			"Stream",
			"Header",
			"Footer",
			"Reference",
                        "Data after EOF",
			NULL
};

char outFileName[MAXNAMELEN];

/****
 *
 * functions
 *
 ****/

/****
 *
 * display pdf object tree
 *
 ****/

int showPdfObjects( struct pdfObject *curPdfObj ) {
  struct pdfObject *curObjPtr, *tmpObjPtr;

#ifdef DEBUG
  if ( config->debug >= 3 )
    display( LOG_DEBUG, "running object chain [0x%08x]  %s p=0x%08x c=0x%08x n=0x%08x", curPdfObj, typeStrings[curPdfObj->type], curPdfObj->parent, curPdfObj->child, curPdfObj->next );
#endif

  curObjPtr = curPdfObj;
  while( curObjPtr != NULL ) {
    if ( curObjPtr->child != NULL ) { /* descend */
      showPdfObjects( curObjPtr->child );
      if ( curObjPtr->next != NULL ) {
	curObjPtr = curObjPtr->next;
      } else {
	return FALSE;
      }
    } else if ( curObjPtr->next != NULL ) { /* traverse */
      curObjPtr = curObjPtr->next;
    } else {
      return FALSE;
    }
  }

  return TRUE;
}

/****
 *
 * load pdf file
 *
 ****/

struct pdfFile *loadFile( char *fname ) {
  FILE *inFile;
  uint8_t inBuf[MAX_BUF_SIZE];
  size_t rCount;
  struct pdfFile *ptrPdf;

  if ( ( ptrPdf = XMALLOC( sizeof( struct pdfFile ) ) ) EQ NULL ) {
    display( LOG_ERR, "Unable to allocate memory" );
    return NULL;
  }
  XMEMSET( ptrPdf, 0, sizeof( struct pdfFile ) );

  if ( ( ptrPdf->fname = XMALLOC( MAXNAMELEN ) ) EQ NULL ) {
      display( LOG_ERR, "Unable to allocate memory" );
      return NULL;
  }
  XSTRNCPY( ptrPdf->fname, fname, MAXNAMELEN-1 );

#ifdef DEBUG
  if ( config->debug >= 5 )
    display( LOG_DEBUG, "Loading [%s] into RAM", ptrPdf->fname );
#endif

  /* open file for read only */
  if ( ( inFile = fopen( ptrPdf->fname, "ro" ) ) EQ NULL ) {
    display( LOG_ERR, "Unable to open [%s] for read", ptrPdf->fname );
    return NULL;
  }

  /* read the file into ram */
  while( ( rCount = fread( inBuf, 1, sizeof( inBuf ), inFile ) ) > 0 ) {
    if ( ( ptrPdf->fileBuf = XREALLOC( ptrPdf->fileBuf, ptrPdf->fileSize + rCount ) ) EQ NULL ) {
      display( LOG_DEBUG, "Unable to grow file buffer" );
      return  NULL;
    }
    XMEMCPY( ptrPdf->fileBuf + ptrPdf->fileSize, inBuf, rCount );
    ptrPdf->fileSize += rCount;
  }

  /* close file */
  fclose( inFile );

#ifdef DEBUG
  if ( config->debug >= 5 )
    display( LOG_DEBUG, "Finished loading file into RAM" );
#endif

  /* done */
  return( ptrPdf );
}

/****
 *
 * parse the pdf file
 *
 ****/

int parseFile( struct pdfFile *ptrPdf ) {
  struct pdfObject *tmpPtr;

  /* alloc base object */
  if ( ( ptrPdf->head = XMALLOC( sizeof( struct pdfObject ) ) ) EQ NULL ) {
    display( LOG_ERR, "Unable to allocate pdf object\n" );
    return FAILED;
  }
  XMEMSET( ptrPdf->head, 0, sizeof( struct pdfObject ) );
  ptrPdf->head->type = -1;

  XSTRNCPY( outFileName, ptrPdf->fname, MAXNAMELEN );

  parsePdfObj( 0, ptrPdf->fileBuf, ptrPdf->fileSize, PDF_TYPE_NONE, ptrPdf->head );

  showPdfObjects( ptrPdf->head );

  /* cleanup object tree */
  while ( ptrPdf->head != NULL ) {
    if ( ptrPdf->head->child != NULL ) {
      /* descend */
      ptrPdf->head = ptrPdf->head->child;
    } else if ( ptrPdf->head->next != NULL ) {
      /* empty this level */
      tmpPtr = ptrPdf->head;
#ifdef DEBUG
      if ( config->debug >= 3 )
	display( LOG_DEBUG, "Removing pdf object from memory chain [%s]", typeStrings[tmpPtr->type] );
#endif
      ptrPdf->head = ptrPdf->head->next;
      XFREE( tmpPtr );
    } else if ( ptrPdf->head->parent != NULL ) {
      /* ascend, removing the child */
      tmpPtr = ptrPdf->head;
#ifdef DEBUG
      if ( config->debug >= 3 )
	display( LOG_DEBUG, "Removing child pdf object from memory chain [%s]", typeStrings[tmpPtr->type] );
#endif
      ptrPdf->head->parent->child = NULL;
      ptrPdf->head = ptrPdf->head->parent;
      XFREE( tmpPtr );
    } else {
      /* at the root, time to stop */
      ptrPdf->head = NULL;
    }
  }
}

/****
 *
 * insert pdf object into memory chain
 *
 ****/

struct pdfObject *insertPdfObject( struct  pdfObject *curObjPtr ) {
  struct pdfObject *tmpObjPtr, *newObjPtr;

#ifdef DEBUG
  if ( config->debug >= 3 )
    display( LOG_DEBUG, "Inserting pdf object" );
#endif

  if ( ( newObjPtr = XMALLOC( sizeof( struct pdfObject ) ) ) EQ NULL ) {
    display( LOG_ERR, "Unable to allocate memory for pdf object" );
    return NULL;
  }
  XMEMSET( newObjPtr, 0, sizeof( struct pdfObject ) );

  if ( curObjPtr->child != NULL ) {
    /* already a child */
    tmpObjPtr = curObjPtr->child;
    while( tmpObjPtr->next != NULL ) {
      tmpObjPtr = tmpObjPtr->next;
    }
    newObjPtr->parent = curObjPtr;
    tmpObjPtr->next = newObjPtr;
  } else {
    /* new child */ 
    newObjPtr->parent = curObjPtr;
    curObjPtr->child = newObjPtr;
  }

#ifdef DEBUG
  if ( config->debug >= 3 )
    display( LOG_DEBUG, "Inserted new object [0x%08x] parent=0x%08x", newObjPtr, newObjPtr->parent );
#endif

  return newObjPtr;
}

/****
 *
 * parse and process pdf object (recursive)
 *
 ****/

size_t parsePdfObj( size_t startPos, uint8_t *buf, size_t endPos, int dataType, struct pdfObject *curObjPtr ) {
  size_t curPos = startPos, bufOffset = 0, sPos;
  uint8_t tmpByte = 0, status = 0, *destBuf;
  int offset = 0, objNum = 0, objRev = 0, count = 0, getNum = 0, i;
  char tmpStr[1024];
  char tmpBuf[4096];
  struct pdfObject *tmpObjPtr;

  /*
   * parse this object
   */

  if ( config->verbose )
    printf( ">> %lu/%lu [%s]\n", startPos, endPos, typeStrings[dataType] );

  /*
   * loop until we are at the end
   */

  while ( curPos < endPos ) {
    if ( dataType EQ PDF_TYPE_NONE ) {
#ifdef DEBUG
      if ( config->debug >= 5 )
	hexDump( curPos, buf+curPos, 64 );
#endif
      if ( buf[curPos] EQ ' ' | buf[curPos] EQ '\r' | buf[curPos] EQ '\n' | buf[curPos] EQ '\t' ) {
	/* white space */
	curPos++;
      } else if ( buf[curPos] EQ '<' ) {
	if ( buf[curPos+1] EQ '<' ) { /* dictionary */
	  curPos+=2;
	  if ( ( curObjPtr = insertPdfObject( curObjPtr ) ) EQ NULL ) {
	    return FAILED;
	  }
	  curObjPtr->type = PDF_TYPE_DICTIONARY;
	  curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_DICTIONARY, curObjPtr );
	} else { /* hex string */
	  curPos++;
	  if ( ( curObjPtr = insertPdfObject( curObjPtr ) ) EQ NULL ) {
	    return FAILED;
	  }
	  curObjPtr->type = PDF_TYPE_HEXSTRING; 
	  curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_HEXSTRING, curObjPtr );
	}
      } else if ( buf[curPos] EQ '(' ) { /* string */
	curPos++;
	if ( ( curObjPtr = insertPdfObject( curObjPtr ) ) EQ NULL ) {
	  return FAILED;
	}
	curObjPtr->type = PDF_TYPE_STRING; 
	curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_STRING, curObjPtr );
      } else if ( buf[curPos] EQ '/' ) { /* label */
	curPos++;
	if ( ( curObjPtr = insertPdfObject( curObjPtr ) ) EQ NULL ) {
	  return FAILED;
	}
	curObjPtr->type = PDF_TYPE_NAME; 
	curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_NAME, curObjPtr );
      } else if ( buf[curPos] EQ '[' ) {
	/* array */
	curPos++;
	if ( ( curObjPtr = insertPdfObject( curObjPtr ) ) EQ NULL ) {
	  return FAILED;
	}
	curObjPtr->type = PDF_TYPE_ARRAY; 
	curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_ARRAY, curObjPtr );
      } else if ( sscanf( buf+curPos, "%d %d %n%s", &objNum, &objRev, &offset, tmpStr ) EQ 3 ) { /* named object */
	if ( strncmp( tmpStr, "obj", 3 ) EQ 0 ) {
	  printf( "Object: %d %d\n", objNum, objRev );
	  curPos += ( offset + 3 );
	  if ( ( curObjPtr = insertPdfObject( curObjPtr ) ) EQ NULL ) {
	    return FAILED;
	  }
	  curObjPtr->type = PDF_TYPE_OBJECT;
	  curObjPtr->num = objNum;
	  curObjPtr->gen = objRev;
	  curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_OBJECT, curObjPtr );
	} else if ( tmpStr[0] EQ 'R' ) {
	  printf( "ObjRef: %d %d\n", objNum, objRev );
	  curPos += ( offset + 1 );
	  if ( ( curObjPtr = insertPdfObject( curObjPtr ) ) EQ NULL ) {
	    return FAILED;
	  }
	  curObjPtr->type = PDF_TYPE_REFERENCE;
	  curObjPtr->num = objNum;
	  curObjPtr->gen = objRev;
	  curObjPtr = curObjPtr->parent;
	  /* once we have loaded all objects, we will link all references to their real objects */
	} else {
	  if ( ( curObjPtr = insertPdfObject( curObjPtr ) ) EQ NULL ) {
	    return FAILED;
	  }
	  curObjPtr->type = PDF_TYPE_INTNUM;
	  curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_INTNUM, curObjPtr );
	}
      } else if ( isdigit( buf[curPos] ) || ( buf[curPos] EQ '-' ) || ( buf[curPos] EQ '+' ) ) { /* number */
	if ( ( curObjPtr = insertPdfObject( curObjPtr ) ) EQ NULL ) {
	  return FAILED;
	}
	curObjPtr->type = PDF_TYPE_INTNUM;
	curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_INTNUM, curObjPtr );
      } else if ( ( buf[curPos] EQ 't' ) || ( buf[curPos] EQ 'f' ) ) { /* boolean number */
	if ( ( curObjPtr = insertPdfObject( curObjPtr ) ) EQ NULL ) {
	  return FAILED;
	}
	curObjPtr->type = PDF_TYPE_BOOL;
	curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_BOOL, curObjPtr );
      } else if ( buf[curPos] EQ '%' ) { /* comment */
	curPos++;
        /* check if this is EOF */
        if ( strncmp( buf+curPos, "%EOF\n", 5 ) EQ 0 ) {
            curPos += 5;
            printf( "EOF\n" );
            curObjPtr->type = PDF_TYPE_EOF;
            curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_EOF, curObjPtr );

        } else {
            if ( ( curObjPtr = insertPdfObject( curObjPtr ) ) EQ NULL ) {
                return FAILED;
            }
            curObjPtr->type = PDF_TYPE_COMMENT;
            curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_COMMENT, curObjPtr );
        }
      } else if ( strncmp( buf+curPos, "stream", 6 ) EQ 0 ) { /* stream */
	curPos += 6;
	if ( buf[curPos] EQ '\r' )
	  curPos++;
	if ( buf[curPos] EQ '\n' )
	  curPos++;
	if ( ( curObjPtr = insertPdfObject( curObjPtr ) ) EQ NULL ) {
	  return FAILED;
	}
	curObjPtr->type = PDF_TYPE_STREAM;
	curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_STREAM, curObjPtr );
      } else if ( strncmp( buf+curPos, "obj", 3 ) EQ 0 ) {
	/* object */
	curPos += 3;
	if ( ( curObjPtr = insertPdfObject( curObjPtr ) ) EQ NULL ) {
	  return FAILED;
	}
	curObjPtr->type = PDF_TYPE_OBJECT;
	curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_OBJECT, curObjPtr );
      } else if ( strncmp( buf+curPos, "xref", 4 ) EQ 0 ) {
          
	/* start of footer */
	curPos += 4;
	if ( ( curObjPtr = insertPdfObject( curObjPtr ) ) EQ NULL ) {
	  return FAILED;
	}
	curObjPtr->type = PDF_TYPE_FOOTER;
	curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_FOOTER, curObjPtr );
        
      } else if ( strncmp( buf+curPos, "startxref", 9 ) EQ 0 ) {
        curPos += 9;  
        curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_NONE, curObjPtr );
      } else if ( strncmp( buf+curPos, "endobj", 6 ) EQ 0 ) {
	return( curPos );

      } else if ( buf[curPos] EQ ']' ) {
	return( curPos );

      } else if ( strncmp( buf+curPos, ">>", 2 ) EQ 0 ) {
	return( curPos );

      } else if ( buf[curPos] EQ '>' ) {
	return( curPos );

      } else {
          if ( isprint( buf[curPos] ) )
              printf( "%c", buf[curPos] );
          else
              printf( "." );
	curPos++;
      }
    } else if ( dataType EQ PDF_TYPE_EOF ) {
        /* all data after EOF is suspect */
	if ( config->write ) {
	  writeStream( curPos, buf+curPos, endPos - curPos );
	} else
	  hexDump( curPos, buf+curPos, endPos - curPos );
        curPos = endPos;
    } else if ( dataType EQ PDF_TYPE_REALNUM ) {
      /* 3.4 = realNum */
      sPos = curPos;
      if ( ( buf[curPos] EQ '-' ) || ( buf[curPos] EQ '+' ) ) {
	tmpStr[curPos-sPos] = buf[curPos];
	curPos++;
      }
      while( isdigit( buf[curPos] ) || buf[curPos] EQ '.' ) {
	tmpStr[curPos-sPos] = buf[curPos];
	curPos++;
      }
      tmpStr[curPos-sPos] = '\0';
      printf( "Real Number: %s\n", safePrint( tmpBuf, sizeof( tmpBuf ), tmpStr ) );
      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[dataType] );
      return( curPos );

    } else if ( dataType EQ PDF_TYPE_INTNUM ) {
      /* 343 = intNum */
      sPos = curPos;

      if ( ( buf[curPos] EQ '-' ) || ( buf[curPos] EQ '+' ) )
	curPos++;

      while( ( isdigit( buf[curPos] ) ) || ( buf[curPos] EQ '.' ) ) {
	if ( buf[curPos] EQ '.' ) {
	  curObjPtr->type = PDF_TYPE_REALNUM;
	  return parsePdfObj( sPos, buf, endPos, PDF_TYPE_REALNUM, curObjPtr );
	}
	curPos++;
      }
      printf( "Integer: %d\n", atoi( buf+sPos ) );
      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[dataType] );
      return( curPos );

    } else if ( dataType EQ PDF_TYPE_BOOL ) {
      /* true | false = bool */

      if ( strncmp( buf+curPos, "true", 4 ) EQ 0 ) {
	/* true */
	printf( "Boolean: true\n" );
	curPos += 4;
      } else if ( strncmp( buf+curPos, "false", 5 ) EQ 0 ) {
	/* false */
	printf( "Boolean: false\n" );
	curPos += 5;
      } else
	curPos++;
      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[dataType] );
      return( curPos );

    } else if ( dataType EQ PDF_TYPE_EOL ) {
      /* \n\r | \n | \n\r\n\r = EOL */
      while( ( buf[curPos] EQ '\n' ) || ( buf[curPos] EQ '\r' ) ) {
	curPos++;
      }
      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[dataType] );
      return( curPos );

    } else if ( dataType EQ PDF_TYPE_COMMENT ) {
      /* % ... EOL = comment */
      sPos = curPos;
      while( ( buf[curPos] != '\r' ) && ( buf[curPos] != '\n' ) ) {
	tmpStr[curPos-sPos] = buf[curPos];
	curPos++;
      }
      tmpStr[curPos-sPos] = '\0';
      printf( "Comment: %s\n", safePrint( tmpBuf, sizeof( tmpBuf ), tmpStr ) );
      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[dataType] );
      return( curPos );

    } else if ( dataType EQ PDF_TYPE_NAME ) {
      /* / ... = name */
      sPos = curPos;
      while( ( buf[curPos] != '\r' ) &&
	     ( buf[curPos] != '\n' ) &&
	     ( buf[curPos] != ' ' ) &&
	     ( buf[curPos] != '[' ) &&
	     ( buf[curPos] != '<' ) &&
	     ( buf[curPos] != '/' ) &&
	     ( buf[curPos] != '(' ) &&
	     ( buf[curPos] != '>' ) &&
	     ( buf[curPos] != ']' ) &&
	     ( buf[curPos] != ')' ) ) {
	/* process label */
	tmpStr[curPos-sPos] = buf[curPos];
	curPos++;
      }
      tmpStr[curPos-sPos] = '\0';
      printf( "Name: %s\n", safePrint( tmpBuf, sizeof( tmpBuf ), tmpStr ) );
      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[dataType] );
      return( curPos );

    } else if ( dataType EQ PDF_TYPE_STRING ) {
      /* ( ... ) = literalString */
      /* XXX add octal notation '\000' */
      /* XXX handle escaped characters */
      while( buf[curPos] EQ ' ' ) {
	curPos++;
      }
      sPos = curPos;
      if ( strncmp( buf+curPos, "254 255", 7 ) EQ 0 ) {
	curObjPtr->type = PDF_TYPE_UTFSTRING;
	return parsePdfObj( curPos, buf, endPos, PDF_TYPE_UTFSTRING, curObjPtr );
      } else if ( ( buf[curPos] EQ 'D' ) && ( buf[curPos+1] EQ ':' ) ) {
	curObjPtr->type = PDF_TYPE_DATESTRING;
	return parsePdfObj( curPos, buf, endPos, PDF_TYPE_DATESTRING, curObjPtr );
      }

      while( buf[curPos] != ')' ) {
	tmpStr[curPos-sPos] = buf[curPos];
	curPos++;
      }
      tmpStr[curPos-sPos] = '\0';
      printf( "String: %s\n", safePrint( tmpBuf, sizeof( tmpBuf ), tmpStr ) );
      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[dataType] );
      return( curPos + 1 );

    } else if ( dataType EQ PDF_TYPE_UTFSTRING ) {
      /* ( 254 255 ... ) = utf encoded literal string */
      while( sscanf( buf+curPos, "%u%n", (unsigned int *)&tmpByte, &offset ) EQ 1 ) {
	curPos += offset;
	if ( buf[curPos] EQ ' ' ) {
	  curPos++;
	}
      }
      while( buf[curPos] != ')' ) {
	curPos++;
      }
      curPos++;
      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[dataType] );
      return( curPos );

    } else if ( dataType EQ PDF_TYPE_DATESTRING ) {
      sPos = curPos;
      /* (D:YYYYMMDDHHmmSSOHH'mm') = date encoded literal string */
      while( buf[curPos] != ')' ) {
	tmpStr[curPos-sPos] = buf[curPos];
	curPos++;
      }
      curPos++;
      tmpStr[curPos-sPos] = '\0';
      printf( "Date String: %s\n", safePrint( tmpBuf, sizeof( tmpBuf ), tmpStr ) );
      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[dataType] );
      return( curPos );

    } else if ( dataType EQ PDF_TYPE_HEXSTRING ) {
      /* < ... > = hexString */
      count = 0;
      sPos = curPos;
      while( buf[curPos] != '>' ) {
	if ( isxdigit( buf[curPos] ) )
	  tmpStr[count++] = buf[curPos];
	curPos++;
      }
      /* append a zero is the string length is odd */
      if ( !( count % 2 ) EQ 0 ) {
	tmpStr[count++] = '0';
      }
      tmpStr[count] = '\0';
      curPos++;
      printf( "HEX String: %s\n", safePrint( tmpBuf, sizeof( tmpBuf ), tmpStr ) );
      for( i = 0, offset = 0; i < count; i += 2 ) {
	tmpStr[offset++] = xtoi( tmpStr+i, 2 );
      }
      hexDump( 0, tmpStr, offset );
      printf( "HEX String: %s\n", safePrint( tmpBuf, sizeof( tmpBuf ), tmpStr ) );
      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[dataType] );
      return( curPos );

    } else if ( dataType EQ PDF_TYPE_UTFHEXSTRING ) {
      /* < fe ff ... > = utf encoded hex string */
      sPos = curPos;


    } else if ( dataType EQ PDF_TYPE_ARRAY ) {
      /* [ ... ] = array NOTE can be an array of any n objects */
      while( buf[curPos] != ']' ) {
	curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_NONE, curObjPtr );
      }
      curPos++;
      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[dataType] );
      return( curPos );

    } else if ( dataType EQ PDF_TYPE_DICTIONARY ) {
      /* << /Kids [ 1 0 R 1 0 R ]  = name tree */
      /*    /Names [ (key1) 1 0 R (key2) 2 0 R ] */
      /*    /Limits [ (firstKey) (lastKey) ] */
      /* << name ... >> = dictionary where key is a name and value can be any object */
      /* XXX organize into name, value pairs */
      while( strncmp( buf+curPos, ">>", 2 ) != 0 ) {
	curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_NONE, curObjPtr );
      }
      curPos += 2;
      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[dataType] );
      return( curPos );
      
    } else if ( dataType EQ PDF_TYPE_OBJECT ) {
      /* 3 0 obj ... endobj = turns any object into an indirect (referencable) object */
      /*                      3 = objNum, 0 = objRev */
      while ( strncmp( buf+curPos, "endobj", 6 ) != 0 ) {
	curPos = parsePdfObj( curPos, buf, endPos, PDF_TYPE_NONE, curObjPtr );
      }
      curPos += 6;
      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[PDF_TYPE_OBJECT] );
      return( curPos );

    } else if ( dataType EQ PDF_TYPE_STREAM ) {
      /* XXX add code to associate the stream dictionary with the stream data */
      /* << ... >>EOLstream\n\r or \r ... \n\r endstream = stream */
      sPos = curPos;

      while( strncmp( buf+curPos, "endstream", 9 ) != 0 ) {
	curPos++;
      }

      printf( "Stream: %lu bytes\n", curPos - sPos );

      /* allocate buffer for decompressed stream */
      if ( ( destBuf = XMALLOC( ( curPos - sPos ) * 4 ) ) EQ NULL ) {
	fprintf( stderr, "ERR - Unable to allocate memory for stream\n" );
	exit( 1 );
      }
      XMEMSET( destBuf, 0, ( curPos-sPos ) * 4 );

      /* decompress the stream */
      z_stream zstrm;
      XMEMSET( &zstrm, 0, sizeof(zstrm) );
      zstrm.avail_in = curPos-sPos;
      zstrm.avail_out = ( curPos-sPos ) * 4;
      zstrm.next_in = buf+sPos;
      zstrm.next_out = destBuf;
      int rsti = inflateInit(&zstrm);
      if (rsti EQ Z_OK) {
	int rst2 = inflate (&zstrm, Z_FINISH);
	if (rst2 >= 0) {
	  if ( config->write ) {
	    writeStream( sPos, destBuf, zstrm.total_out );
	  } else
	    hexDump( 0, destBuf, zstrm.total_out );
	} else {
	  printf( "Stream did not decompress\n" );
	  if ( config->write ) {
	    writeStream( sPos, buf+sPos, curPos-sPos );
	  } else
	    hexDump( sPos, buf+sPos, curPos-sPos );
	}
      }

      XFREE( destBuf );

      curPos += 9;

      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[dataType] );
      return( curPos );
      
    } else if ( dataType EQ PDF_TYPE_HEADER ) {
      /* %PDF-1.6EOL%nnnnEOL = file header where each n is > 128 to make file look binary */

    } else if ( dataType EQ PDF_TYPE_FOOTER ) {
      /* xref ... trailer ... startxref ... %%EOF = document footer */
      /* XXX making an asumption, big mistake */
      
      /* 0 271 */
      /* 0000000000 65535 f */
      /* 0000000015 00000 n */
      
      /* found the xref, now look for the trailer, startxref and finally the EOF */
      
      fprintf( stderr, "PDF Footer\n" );
      
      /* XXX need to parse xref table, may be multiple tables in the file */
      
      if ( strncmp( buf+curPos, "trailer", 7 ) EQ 0 ) {
          printf( "trailer\n" );
          curPos += 7;
      } else
          curPos++;
      
      if ( config->verbose )
	printf( "<< %lu/%lu [%s]\n", curPos, endPos, typeStrings[dataType] );
      
      return( curPos );
    }
  }

  return( curPos );
}

/****
 *
 * write data stream to disk
 *
 ****/

size_t writeStream( size_t curPos, uint8_t *buf, size_t len ) {
  FILE *outFile = NULL;
  char tmpFileName[MAXNAMELEN];
  size_t wCount;

  sprintf( tmpFileName, "%s.pdfcarve.%lu", outFileName, curPos );
  fprintf( stderr, "Saving object to [%s]\n", tmpFileName );
  
  if ( ( outFile = fopen( tmpFileName, "w" ) ) EQ NULL ) {
    fprintf( stderr, "ERR - Unable to open file for stream write [%s]\n", tmpFileName );
    return( 0 );
  }

  wCount = fwrite( buf, 1, len, outFile );

  fclose( outFile );

  return( wCount );
}

/****
 *
 * parse an object
 *
 ****/

struct pdfObject *parseObject( int inFile, off_t offset ) {
  
}

/****
 *
 * Converts a hexadecimal string to integer
 *
 ****/

unsigned int xtoi( const char* xs, int maxSize ) {
  unsigned int result = 0;
  size_t szlen = strlen( xs );
  int i, xv, fact;

  if ( ( maxSize > 0 ) && ( maxSize <= szlen ) )
    szlen = maxSize;

  if ( szlen > 0 ) {
    // Converting more than 32bit hexadecimal value?
    if ( szlen > 8 )
      return 0; // exit

    // Begin conversion here
    fact = 1;

    // Run until no more character to convert
    for( i = szlen - 1; i >= 0 ;i-- ) {
      if ( isxdigit( *( xs + i ) ) ) {
	if ( *( xs + i ) >= 97 ) {
	  xv = ( *( xs + i ) - 97 ) + 10;
	} else if ( *( xs + i ) >= 65 ) {
	  xv = ( *( xs + i ) - 65 ) + 10;
	} else {
	  xv = *( xs + i ) - 48;
	}
	result += ( xv * fact );
	fact *= 16;
      } else {
	/* got a non-hex token, return what we have so far */
	return result;
      }
    }
  }

  return result;
}

/****
 *
 * safe print string
 *
 ****/

char *safePrint( char *outBuf, int outBufSize, char *inBuf ) {
    int i;
    
    XMEMSET( outBuf, 0, outBufSize );
    
    for( i = 0; ( inBuf[i] != 0 ) && ( i < outBufSize ); i++ ) {
        if ( isprint( inBuf[i] ) )
            outBuf[i] = inBuf[i];
        else
            outBuf[i] = '.';
    }
    
    return outBuf;
}

/****
 *
 * hexdump
 *
 ****/

void hexDump( size_t bPos, uint8_t buf[], size_t len ) {
  size_t y, i = 0;

#ifdef DEBUG
  if ( config->debug >= 7 )
    display( LOG_DEBUG, "%d %d", bPos, len );
#endif

  while ( i < len ) {
    printf( "%08lx ", bPos + i );
    for ( y = 0; y < 16 & i + y < len; y++ ) {

      printf( "%02x", buf[i+y] );
      printf( " " );
    }
    while( y < 16 ) {
      printf( "   " );
      y++;
    }
    printf( " " );
    for ( y = 0; y < 16 & i + y < len; y++ ) {
      if ( ( buf[i+y] < 32 ) | ( buf[i+y] > 127 ) )
        printf( "." );
      else
        printf( "%c", buf[i+y] );
    }
    i += y;

    printf( "\n" );
  }
}
