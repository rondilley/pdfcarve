/*****
 *
 * Description: PDF parser headers
 * 
 * Copyright (c) 2008-2025, Ron Dilley
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

#ifndef PARSER_DOT_H
#define PARSER_DOT_H

/****
 *
 * includes
 *
 ****/

#include "main.h"

/****
 *
 * defines
 *
 ****/

#define MAX_BUF_SIZE 4096

/****
 *
 * typdefs & structs
 *
 ****/

struct boolObj {

};

struct intObj {

};

struct realObj {

};

struct stringObj {

};

struct nameObj {

};

struct arrayObj {

};

struct dictObj {

};

struct streamObj {

};

struct pdfObject {
  int num;
  int gen;
  int type;
  int size;
  void *buf;
  struct pdfObject *parent;
  struct pdfObject *child;
  struct pdfObject *next;
};

struct pdfFile {
  char *fname;
  size_t fileSize;
  uint8_t *fileBuf;
  struct pdfObject *head;
};

/****
 *
 * function prototypes
 *
 ****/
struct pdfFile *loadFile( char *fname );
int parseFile( struct pdfFile *ptrPdf );
size_t parsePdfObj( size_t startPos, uint8_t *buf, size_t endPos, int dataType, struct pdfObject *curObjPtr, int depth );
struct pdfObject *parseObject( int inFile, off_t offset );
char *safePrint( char *outBuf, int outBufSize, char *inBuf );
void hexDump( size_t bPos, uint8_t buf[], size_t len );
size_t writeStream( size_t curPos, uint8_t *buf, size_t len );
void cleanupPdfObjectTree( struct pdfObject *root );
unsigned int xtoi( const char* xs, int maxSize );

#endif /* end of PARSER_DOT_H */

