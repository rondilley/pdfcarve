/*****
 *
 * Description: Memory Helper Functions
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

/****
 *
 * defines
 *
 ****/

/****
 *
 * includes
 *
 ****/

#include "mem.h"

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

extern int quit;

/****
 *
 * global variables
 *
 ****/

#ifdef MEM_DEBUG
PRIVATE struct Mem_s *head;
PRIVATE struct Mem_s *tail;
#endif

/****
 *
 * functions
 *
 ****/

/****
 *
 * Copy argv into a newly malloced buffer.  Arguments are concatenated
 * with spaces in between each argument.
 *
 ****/

PUBLIC char *copy_argv(char *argv[]) {
  PRIVATE char **arg;
  PRIVATE char *buf;
  PRIVATE int total_length = 0;

  for (arg = argv; *arg != NULL; arg++) {
    total_length += (strlen(*arg) + 1); /* length of arg plus space */
  }

  if (total_length == 0)
    return NULL;

  total_length++; /* add room for a null */

  buf = ( char * )XMALLOC( sizeof( char ) * total_length);

  *buf = 0;
  for (arg = argv; *arg != NULL; arg++) {
#ifdef HAVE_STRNCAT
    strncat(buf, *arg, total_length );
    strncat(buf, " ", total_length );
#else
    strlcat(buf, *arg, total_length );
    strlcat(buf, " ", total_length );
#endif
  }

  return buf;
}

/****
 *
 * Allocate memory. Checks the return value, aborts if no more memory is available
 *
 ****/

void *xmalloc_( const int size, const char *filename, const int linenumber) {
  void *result;
#ifdef MEM_DEBUG
  PRIVATE struct Mem_s *d_result;
#endif

  /* allocate buf */
  result = malloc( size );
  if ( result EQ NULL ) {
    fprintf( stderr, "out of memory (%d at %s:%d)!\n", size, filename, linenumber );
#ifdef MEM_DEBUG
    XFREE_ALL();
#endif
    exit( 1 );
  }

#ifdef MEM_DEBUG
  d_result = malloc( sizeof(struct Mem_s) );
  if ( d_result EQ NULL ) {
    fprintf( stderr, "out of memory (%ld at %s:%d)!\n", sizeof(struct Mem_s), filename, linenumber );
    XFREE_ALL();
    exit( 1 );
  }
  /* clean it */
  bzero( d_result, sizeof(struct Mem_s) );

#ifdef SHOW_MEM_DEBUG
  fprintf( stderr, "0x%016lx malloc() called from %s:%d (%d bytes)\n", (unsigned long)result, filename, linenumber, size );
#endif

  /* link into the buffer chain */
  if ( tail EQ NULL ) {
    head = d_result;
    tail = d_result;
  } else {
    tail->next = d_result;
    d_result->prev = tail;
    d_result->next = NULL;
    tail = d_result;
  }

  /* associate the debug object with the object */
  d_result->buf_ptr = (void *)result;
  d_result->buf_size = size;
#endif

  bzero( result, size );

#ifdef MEM_DEBUG
  d_result->status = MEM_D_STAT_CLEAN;
#endif

  return result;
}

/****
 *
 * copy from one place to another
 *
 ****/

void *xmemcpy_( void *d_ptr, void *s_ptr, const int size, const char *filename, const int linenumber ) {
  void *result;
#ifdef MEM_DEBUG
  PRIVATE struct Mem_s *mem_ptr;
  PRIVATE int source_size;
  PRIVATE int dest_size;
#endif

  if ( s_ptr EQ NULL ) {
    fprintf( stderr, "memcpy called with NULL source pointer at %s:%d\n", filename, linenumber );
#ifdef MEM_DEBUG
    XFREE_ALL();
#endif
    exit( 1 );
  }
  if ( d_ptr EQ NULL ) {
    fprintf( stderr, "memcpy called with NULL dest pointer at %s:%d\n", filename, linenumber );
#ifdef MEM_DEBUG
    XFREE_ALL();
#endif
    exit( 1 );
  }

#ifdef MEM_DEBUG
  /* search for debug mem objects */
  source_size = dest_size = 0;
  mem_ptr = head;
  while( mem_ptr != NULL ) {
    if ( mem_ptr->buf_ptr EQ d_ptr ) {
      /* found the dest */
      dest_size = mem_ptr->buf_size;
    } else if ( mem_ptr->buf_ptr EQ s_ptr ) {
      /* found the source */
      source_size = mem_ptr->buf_size;
    }
    mem_ptr = mem_ptr->next;
  }

  if ( dest_size > 0 ) {
    if ( dest_size < size ) {
      /* attempting to copy too much data into dest */
      fprintf( stderr, "memcpy called with size (%d) larger than dest buffer 0x%08lx (%d) at %s:%d\n", size, (unsigned long)d_ptr, dest_size, filename, linenumber );
      XFREE_ALL();
      exit( 1 );
    }

    if ( source_size > 0 ) {
      if ( source_size < size ) {
        /* attempting to copy too much data from source */
        fprintf( stderr, "memcpy called with size (%d) larger than source buffer 0x%08lx (%d) at %s:%d\n", size, (unsigned long)s_ptr, source_size, filename, linenumber );
        XFREE_ALL();
        exit( 1 );
      }
    } else {
      /* could not find source buffer */
#ifdef SHOW_MEM_DEBUG
      fprintf( stderr, "0x%016lx could not find source buffer at %s:%d called from %s:%d\n", (unsigned long)s_ptr, __FILE__, __LINE__, filename, linenumber );
#endif
    }
  } else {
    /* could not find dest buffer */
#ifdef SHOW_MEM_DEBUG
    fprintf( stderr, "0x%016lx could not find dest buffer at %s:%d called from %s:%d\n", (unsigned long)d_ptr, __FILE__, __LINE__, filename, linenumber );
#endif
  }
#endif

  if ( s_ptr < d_ptr ) {
    if ( s_ptr + size >= d_ptr ) {
      /* overlap, use memmove */
      result = memmove( d_ptr, s_ptr, size );
    } else {
      /* no overlap, use memcpy */
      result = memcpy( d_ptr, s_ptr, size );
    }
  } else if ( s_ptr > d_ptr ) {
    if ( d_ptr + size >= s_ptr ) {
      /* overlap, use memmove */
      result = memmove( d_ptr, s_ptr, size );
    } else {
      /* no overlap, use memcpy */
      result = memcpy( d_ptr, s_ptr, size );
    }
  } else {
    /* source and dest are the same, freak out */
    fprintf( stderr, "memcpy() called with source EQ dest at %s:%d\n", filename, linenumber );
#ifdef MEM_DEBUG
    XFREE_ALL();
#endif
    exit( 1 );
  }

#ifdef SHOW_MEM_DEBUG
  fprintf( stderr, "0x%016lx memcpy() called from %s:%d (%d bytes)\n", (unsigned long)result, filename, linenumber, size );
#endif

  return result;
}

/****
 *
 * copy from one place to another
 *
 ****/

char *xmemncpy_( char *d_ptr, const char *s_ptr, const size_t len, const int size, const char *filename, const int linenumber ) {
  char *result;
#ifdef MEM_DEBUG
  PRIVATE struct Mem_s *mem_ptr;
  PRIVATE int source_size;
  PRIVATE int dest_size;
#endif

  if ( s_ptr EQ NULL ) {
    fprintf( stderr, "memcpy called with NULL source pointer at %s:%d\n", filename, linenumber );
#ifdef MEM_DEBUG
    XFREE_ALL();
#endif
    exit( 1 );
  }
  if ( d_ptr EQ NULL ) {
    fprintf( stderr, "memcpy called with NULL dest pointer at %s:%d\n", filename, linenumber );
#ifdef MEM_DEBUG
    XFREE_ALL();
#endif
    exit( 1 );
  }

#ifdef MEM_DEBUG
  /* search for debug mem objects */
  source_size = dest_size = 0;
  mem_ptr = head;
  while( mem_ptr != NULL ) {
    if ( mem_ptr->buf_ptr EQ d_ptr ) {
      /* found the dest */
      dest_size = mem_ptr->buf_size;
    } else if ( mem_ptr->buf_ptr EQ s_ptr ) {
      /* found the source */
      source_size = mem_ptr->buf_size;
    }
    mem_ptr = mem_ptr->next;
  }

  if ( dest_size > 0 ) {
    if ( dest_size < size ) {
      /* attempting to copy too much data into dest */
      fprintf( stderr, "memcpy called with size (%d) larger than dest buffer 0x%08lx (%d) at %s:%d\n", size, (unsigned long)d_ptr, dest_size, filename, linenumber );
      XFREE_ALL();
      exit( 1 );
    }

    if ( source_size > 0 ) {
      if ( source_size < size ) {
        /* attempting to copy too much data from source */
        fprintf( stderr, "memcpy called with size (%d) larger than source buffer 0x%08lx (%d) at %s:%d\n", size, (unsigned long)s_ptr, source_size, filename, linenumber );
        XFREE_ALL();
        exit( 1 );
      }
    } else {
      /* could not find source buffer */
#ifdef SHOW_MEM_DEBUG
      fprintf( stderr, "0x%016lx could not find source buffer at %s:%d called from %s%d\n", (unsigned long)s_ptr, __FILE__, __LINE__, filename, linenumber );
#endif
    }
  } else {
    /* could not find dest buffer */
#ifdef SHOW_MEM_DEBUG
    fprintf( stderr, "0x%016lx could not find dest buffer at %s:%d called from %s:%d\n", (unsigned long)d_ptr, __FILE__, __LINE__, filename, linenumber );
#endif
  }
#endif

  if ( s_ptr < d_ptr ) {
    if ( s_ptr + size >= d_ptr ) {
      /* overlap, use memmove */
      result = memmove( d_ptr, s_ptr, size );
    } else {
      /* no overlap, use memcpy */
      result = memcpy( d_ptr, s_ptr, size );
    }
  } else if ( s_ptr > d_ptr ) {
    if ( d_ptr + size >= s_ptr ) {
      /* overlap, use memmove */
      result = memmove( d_ptr, s_ptr, size );
    } else {
      /* no overlap, use memcpy */
      result = memcpy( d_ptr, s_ptr, size );
    }
  } else {
    /* source and dest are the same, freak out */
    fprintf( stderr, "memcpy() called with source EQ dest at %s:%d\n", filename, linenumber );
#ifdef MEM_DEBUG
    XFREE_ALL();
#endif
    exit( 1 );
  }

#ifdef SHOW_MEM_DEBUG
  fprintf( stderr, "0x%016lx memcpy() called from %s:%d (%d bytes)\n", (unsigned long)result, filename, linenumber, size );
#endif

  return result;
}

/****
 *
 * set memory area
 *
 ****/

void *xmemset_( void *ptr, const char value, const int size, const char *filename, const int linenumber ) {
  void *result;

  if ( ptr EQ NULL ) {
    fprintf( stderr, "memset() called with NULL ptr at %s:%d\n", filename, linenumber );
    quit = TRUE;
    exit( 1 );
  }

  if ( value EQ 0 ) {
    bzero( ptr, size );
    result = ptr;
  } else {
    result = memset( ptr, value, size );
  }

#ifdef DEBUG_MEM
  fprintf( stderr, "0x%016lx memset %s:%d (%d bytes)\n", (unsigned long)result, filename, linenumber, size);
#endif

  return result;
}

/****
 *
 * Allocate memory. Checks the return value, aborts if no more memory is available
 *
 ****/

void *xrealloc_( void *ptr, int size, const char *filename, const int linenumber) {
  void *result;
  int current_size, found = FALSE;
  struct Mem_s *mem_ptr;
  
  if ( ptr EQ NULL ) {
    return XMALLOC( size );
  }

  if ( size <= 0 ) {
    fprintf( stderr, "realloc() called with invalid size [%d] at %s:%d\n", size, filename, linenumber );
    quit = TRUE;
    exit( 1 );
  }

#ifdef MEM_DEBUG
  /* search for debug mem objects */
  mem_ptr = head;
  while( mem_ptr != NULL ) {
    if ( mem_ptr->buf_ptr EQ ptr ) {
      /* found the dest */
      current_size = mem_ptr->buf_size;
      found = TRUE;
      break;
    }
    mem_ptr = mem_ptr->next;
  }

  if ( found ) {
    if ( size > current_size ) {
      /* growing the buffer */
#ifdef SHOW_MEM_DEBUG
    fprintf( stderr, "0x%016lx growing buffer from [%d] to [%d] at %s:%d called from %s:%d\n", (unsigned long)ptr, current_size, size, __FILE__, __LINE__, filename, linenumber );
#endif
    } else if ( size < current_size ) {
      /* shringing the buffer */
#ifdef SHOW_MEM_DEBUG
    fprintf( stderr, "0x%016lx could not find current buffer at %s:%d called from %s:%d\n", (unsigned long)ptr, __FILE__, __LINE__, filename, linenumber );
#endif
    } else {
      /* no change, just return */
      return( ptr );
    }
  } else {
#ifdef SHOW_MEM_DEBUG
    fprintf( stderr, "0x%016lx could not find current buffer at %s:%d called from %s:%d\n", (unsigned long)ptr, __FILE__, __LINE__, filename, linenumber );
#endif      
  }
#endif

  if ( ( result = realloc( ptr, size ) ) != NULL ) {
#ifdef MEM_DEBUG
      if ( found ) {
          /* check to see if the ptr changed */
        mem_ptr->buf_size = size;
      } else {
            /* not found, need to add the ptr to the tracker */
      }
#endif
  } else {
    fprintf( stderr, "out of memory (%d at %s:%d)!\n", size, filename, linenumber );
    quit = TRUE;
    exit( 1 );   
  }

#ifdef MEM_DEBUG
  fprintf( stderr, "0x%016lx realloc %s:%d (%d bytes)\n", (unsigned long)result, filename, linenumber, size);
#endif

  return result;
}

/****
 *
 * Free memory. Merely a wrapper for the case that we want to keep track of allocations.
 *
 ****/

void xfree_( void *ptr, const char *filename, const int linenumber ) {
#ifdef MEM_DEBUG
  PRIVATE struct Mem_s *d_ptr;
  PRIVATE int found = FALSE;
  PRIVATE int size = 0;
#endif

  if ( ptr EQ NULL ) {
    fprintf( stderr, "free() called with NULL ptr at %s:%d\n", filename, linenumber );
    exit( 1 );
  }

#ifdef MEM_DEBUG
  d_ptr = head;
  while( d_ptr != NULL ) {
    if ( d_ptr->buf_ptr EQ ptr ) {
      /* found debug object */
      found = TRUE;
      if ( d_ptr->prev != NULL ) {
        d_ptr->prev->next = d_ptr->next;
      } else {
        head = (void *)d_ptr->next;
      }
      if ( d_ptr->next != NULL ) {
        d_ptr->next->prev = d_ptr->prev;
      } else {
        tail = d_ptr->prev;
      }
      size = d_ptr->buf_size;
      free( d_ptr );
      d_ptr = NULL;
    } else {
      d_ptr = d_ptr->next;
    }
  }

  if ( ! found ) {
    fprintf( stderr, "free() called with 0x%08lx ptr but not found in debug object list at %s:%d\n", (unsigned long)ptr, filename, linenumber );
    return;
  }
#endif

#ifdef SHOW_MEM_DEBUG
#ifdef MEM_DEBUG
  fprintf( stderr, "0x%016lx free() called from %s:%d (%d bytes)\n", (unsigned long)ptr, filename, linenumber, size );
#else
  fprintf( stderr, "0x%016lx free() called from %s:%d\n", (unsigned long)ptr, filename, linenumber );
#endif
#endif

  free( ptr );
}

/****
 *
 * free all known buffers
 *
 ****/

#ifdef MEM_DEBUG
void xfree_all_( const char *filename, const int linenumber ) {
  PRIVATE struct Mem_s *d_ptr;
  PRIVATE int size = 0;
  PRIVATE struct timespec pause_time;

  /* mutex trylock pause */
  pause_time.tv_sec = 0;
  pause_time.tv_nsec = 500;

#ifdef SHOW_MEM_DEBUG
  fprintf( stderr, "xfree_all() called from %s:%d\n", filename, linenumber );
#endif

  /* pop all buffers off the list */
  while( ( d_ptr = head ) != NULL ) {
    head = d_ptr->next;
    if ( d_ptr->buf_ptr != NULL ) {
#ifdef SHOW_MEM_DEBUG
      fprintf( stderr, "0x%016lx free %s:%d (%d bytes)\n", (unsigned long)d_ptr->buf_ptr, __FILE__, __LINE__, d_ptr->buf_size );
#endif
      free( d_ptr->buf_ptr );
    }
    free( d_ptr );
  }

  return;
}
#endif

/****
 *
 * Dup a string
 *
 ****/

char *xstrdup_( const char *str, const char *filename, const int linenumber ) {
  char *res;

  res = strdup( str );

#ifdef DEBUG_MEM
  fprintf( stderr, "0x%016lx malloc %s:%d (%d bytes, strdup)\n", (unsigned long)res, filename, linenumber, strlen(str)+1 );
#endif

  return res;
}

/****
 *
 * grow or shrink an array
 *
 ****/

void xgrow_( void **old, int elementSize, int *oldCount, int newCount, char *filename, const int linenumber ) {
  void *tmp;
  int size;

  size = newCount * elementSize;
  if ( size EQ 0 )
    tmp = NULL;
  else {
    tmp = malloc(size);

#ifdef DEBUG_MEM
    fprintf( stderr, "0x%016lx malloc %s:%d (grow)\n", (unsigned long)tmp, filename, linenumber );
#endif

  if ( tmp EQ NULL ) {
    fprintf( stderr, "out of memory (%d at %s:%d)!\n", size, filename, linenumber );
    quit = TRUE;
    exit( 1 );
  }
  memset( tmp, 0, size );
  if ( *oldCount > newCount )
    *oldCount = newCount;
    memcpy(tmp, *old, elementSize * ( *oldCount ) );
  }

  if ( *old != NULL ) {
#ifdef DEBUG_MEM
    fprintf( stderr, "0x%016lx free %s:%d (grow)\n", (unsigned long) *old, filename, linenumber );
#endif
    free( *old );
  }
  *old = tmp;
  *oldCount = newCount;
}

/****
 *
 * wraper around strcpy
 *
 ****/

char *xstrcpy_( char *d_ptr, const char *s_ptr, const char *filename, const int linenumber ) {
  void *result;
  PRIVATE int size;
#ifdef MEM_DEBUG
  PRIVATE struct Mem_s *mem_ptr;
  PRIVATE int source_size;
  PRIVATE int dest_size;
#endif

  if ( s_ptr EQ NULL ) {
    fprintf( stderr, "strcpy called with NULL source pointer at %s:%d\n", filename, linenumber );
#ifdef MEM_DEBUG
    XFREE_ALL();
#endif
    exit( 1 );
  }
  if ( d_ptr EQ NULL ) {
    fprintf( stderr, "strcpy called with NULL dest pointer at %s:%d\n", filename, linenumber );
#ifdef MEM_DEBUG
    XFREE_ALL();
#endif
    exit( 1 );
  }

  if ( ( size = (strlen( s_ptr )+1)) EQ 0 ) {
#ifdef SHOW_MEM_DEBUG
    fprintf( stderr, "strcpy called with zero length source pointer at %s:%d\n", filename, linenumber );
#endif
    d_ptr[0] = 0;
    return d_ptr;
  }

#ifdef MEM_DEBUG
  /* search for debug mem objects */
  source_size = dest_size = 0;
  mem_ptr = head;
  while( mem_ptr != NULL ) {
    if ( mem_ptr->buf_ptr EQ d_ptr ) {
      /* found the dest */
      dest_size = mem_ptr->buf_size;
    } else if ( mem_ptr->buf_ptr EQ s_ptr ) {
      /* found the source */
      source_size = mem_ptr->buf_size;
    }
    mem_ptr = mem_ptr->next;
  }

  if ( dest_size > 0 ) {
    if ( dest_size < size ) {
      /* attempting to copy too much data into dest */
      fprintf( stderr, "strcpy called with size (%d) larger than dest buffer 0x%08lx (%d) at %s:%d\n", size, (unsigned long)d_ptr, dest_size, filename, linenumber );
      XFREE_ALL();
      exit( 1 );
    }

    if ( source_size > 0 ) {
      if ( source_size < size ) {
        /* attempting to copy too much data from source */
        fprintf( stderr, "strcpy called with size (%d) larger than source buffer 0x%08lx (%d) at %s:%d\n", size, (unsigned long)s_ptr, source_size, filename, linenumber );
        XFREE_ALL();
        exit( 1 );
      }
    } else {
      /* could not find source buffer */
#ifdef SHOW_MEM_DEBUG
      fprintf( stderr, "0x%016lx could not find source buffer at %s:%d called from %s%d\n", (unsigned long)s_ptr, __FILE__, __LINE__, filename, linenumber );
#endif
    }
  } else {
    /* could not find dest buffer */
#ifdef SHOW_MEM_DEBUG
    fprintf( stderr, "0x%016lx could not find dest buffer at %s:%d called from %s:%d\n", (unsigned long)d_ptr, __FILE__, __LINE__, filename, linenumber );
#endif
  }
#endif

  if ( s_ptr < d_ptr ) {
    if ( s_ptr + size >= d_ptr ) {
      /* overlap, use memmove */
      result = memmove( d_ptr, s_ptr, size );
    } else {
      /* no overlap, use memcpy */
      result = memcpy( d_ptr, s_ptr, size );
    }
  } else if ( s_ptr > d_ptr ) {
    if ( d_ptr + size >= s_ptr ) {
      /* overlap, use memmove */
      result = memmove( d_ptr, s_ptr, size );
    } else {
      /* no overlap, use memcpy */
      result = memcpy( d_ptr, s_ptr, size );
    }
  } else {
    /* source and dest are the same, freak out */
    fprintf( stderr, "strcpy() called with source EQ dest at %s:%d\n", filename, linenumber );
#ifdef MEM_DEBUG
    XFREE_ALL();
#endif
    exit( 1 );
  }
  d_ptr[size-1] = 0;

#ifdef SHOW_MEM_DEBUG
  fprintf( stderr, "0x%016lx strcpy() called from %s:%d (%d bytes)\n", (unsigned long)result, filename, linenumber, size );
#endif

  return result;
}

/****
 *
 * wraper around strncpy
 *
 ****/

char *xstrncpy_( char *d_ptr, const char *s_ptr, const size_t len, const char *filename, const int linenumber ) {
  char *result;
  PRIVATE int size;
#ifdef MEM_DEBUG
  PRIVATE struct Mem_s *mem_ptr;
  PRIVATE int source_size;
  PRIVATE int dest_size;
#endif

  /* check for null source pointer */
  if ( s_ptr EQ NULL ) {
    fprintf( stderr, "strncpy called with NULL source pointer at %s:%d\n", filename, linenumber );
#ifdef MEM_DEBUG
    XFREE_ALL();
#endif
    exit( 1 );
  }

  /* check for null dest pointer */
  if ( d_ptr EQ NULL ) {
    fprintf( stderr, "strncpy called with NULL dest pointer at %s:%d\n", filename, linenumber );
#ifdef MEM_DEBUG
    XFREE_ALL();
#endif
    exit( 1 );
  }

  /* check size of len arg */
  if ( len EQ 0 ) {
#ifdef SHOW_MEM_DEBUG
    fprintf( stderr, "strncpy called with zero copy length at %s:%d\n", filename, linenumber );
#endif
    d_ptr[0] = 0;
    return d_ptr;
  }

  /* check size of source string */
  if ( ( size = ( strlen( s_ptr ) ) ) EQ 0 ) {
#ifdef SHOW_MEM_DEBUG
    fprintf( stderr, "strncpy called with zero length source pointer at %s:%d\n", filename, linenumber );
#endif
    d_ptr[0] = 0;
    return d_ptr;
  }

  /* check if source string lenght >= length arg */
  if ( size >= len ) {
#ifdef SHOW_MEM_DEBUG
    fprintf( stderr, "strncpy called with source string >= length arg at %s:%d\n", filename, linenumber );
#endif
  }

#ifdef MEM_DEBUG
  /* search for debug mem objects */
  source_size = dest_size = 0;
  mem_ptr = head;
  while( mem_ptr != NULL ) {
    if ( mem_ptr->buf_ptr EQ d_ptr ) {
      /* found the dest */
      dest_size = mem_ptr->buf_size;
    } else if ( mem_ptr->buf_ptr EQ s_ptr ) {
      /* found the source */
      source_size = mem_ptr->buf_size;
    }
    mem_ptr = mem_ptr->next;
  }

  if ( dest_size > 0 ) {
    if ( dest_size < len ) {
      /* attempting to copy too much data into dest */
      fprintf( stderr, "strncpy called with size (%ld) larger than dest buffer 0x%08lx (%d) at %s:%d\n", len, (unsigned long)d_ptr, dest_size, filename, linenumber );
      XFREE_ALL();
      exit( 1 );
    }

    if ( source_size > 0 ) {
      if ( source_size < len ) {
        /* attempting to copy too much data from source */
        fprintf( stderr, "strncpy called with size (%ld) larger than source buffer 0x%08lx (%d) at %s:%d\n", len, (unsigned long)s_ptr, source_size, filename, linenumber );
        XFREE_ALL();
        exit( 1 );
      }
    } else {
      /* could not find source buffer */
#ifdef SHOW_MEM_DEBUG
      fprintf( stderr, "0x%016lx could not find source buffer at %s:%d called from %s:%d\n", (unsigned long)s_ptr, __FILE__, __LINE__, filename, linenumber );
#endif
    }
  } else {
    /* could not find dest buffer */
#ifdef SHOW_MEM_DEBUG
    fprintf( stderr, "0x%016lx could not find dest buffer at %s:%d called from %s:%d\n", (unsigned long)d_ptr, __FILE__, __LINE__, filename, linenumber );
#endif
  }
#endif

  if ( d_ptr != s_ptr ) {
    result = strncpy( d_ptr, s_ptr, len );
  } else {
    /* source and dest are the same, freak out */
    fprintf( stderr, "strncpy() called with source EQ dest at %s:%d\n", filename, linenumber );
#ifdef MEM_DEBUG
    XFREE_ALL();
#endif
    exit( 1 );
  }

#ifdef SHOW_MEM_DEBUG
  fprintf( stderr, "0x%016lx strncpy() called from %s:%d (%d bytes)\n", (unsigned long)result, filename, linenumber, size );
#endif

  return result;
}

