/*****
 *
 * Description: Utility Functions
 * 
 * Copyright (c) 2009-2025, Ron Dilley
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

/* turn on priority names */
#define SYSLOG_NAMES

/****
 *
 * includes
 *
 ****/

#include "util.h"

/****
 *
 * local variables
 *
 ****/

PRIVATE char *restricted_environ[] = {
  "IFS= \t\n",
  "PATH= /bin:/usr/bin",
  0
};
PRIVATE char *preserve_environ[] = {
  "TZ",
  0
};

/****
 *
 * external global variables
 *
 ****/

extern Config_t *config;
extern char **environ;

/****
 *
 * functions
 *
 ****/

/****
 *
 * check to see if dir is safe
 *
 ****/
int is_dir_safe( const char *dir ) {
  DIR *fd, *start;
  int rc = FAILED;
  char new_dir[PATH_MAX+1];
  uid_t uid;
  struct stat f, l;

  if ( !( start = opendir( "." ) ) ) return FAILED;
  if ( lstat( dir, &l ) == FAILED ) {
    closedir( start );
    return FAILED;
  }
  uid = geteuid();

  do {
    if ( chdir( dir ) EQ FAILED ) break;
    if ( !( fd = opendir( "." ) ) ) break;

#ifdef LINUX
    if ( fstat( dirfd( fd ), &f ) EQ FAILED ) {
#elif defined MACOS
    if ( fstat( fd->__dd_fd, &f ) EQ FAILED ) {
#elif defined CYGWIN
    if ( fstat( fd->__d_fd, &f ) EQ FAILED ) {
#elif defined MINGW
    if ( fstat( fd->dd_handle, &f ) EQ FAILED ) {
#elif defined FREEBSD
    if ( fstat( dirfd( fd ), &f ) EQ FAILED ) { 
#elif defined OPENBSD
    if ( fstat( dirfd( fd ), &f ) EQ FAILED ) { 
#else
    if ( fstat( fd->dd_fd, &f ) EQ FAILED ) {
#endif

      closedir( fd );
      break;
    }
    closedir( fd );

    if ( l.st_mode != f.st_mode || l.st_ino != f.st_ino || l.st_dev != f.st_dev )
      break;
#ifdef MINGW
	if ( f.st_uid && f.st_uid != uid ) {
#else
    if ( ( f.st_mode & ( S_IWOTH | S_IWGRP ) ) || ( f.st_uid && f.st_uid != uid ) ) {
#endif
      rc = 0;
      break;
    }
    dir = "..";
    if ( lstat( dir, &l ) EQ FAILED ) break;
    if ( !getcwd( new_dir, PATH_MAX + 1 ) ) break;
  } while ( new_dir[1] ); /* new_dir[0] will always be a slash */
  if ( !new_dir[1] ) rc = 1;

#ifdef LINUX
  rc = fchdir( dirfd( start ) );
#elif defined MACOS
  rc = fchdir( start->__dd_fd );
#elif defined CYGWIN
  rc = fchdir( start->__d_fd );
#elif defined MINGW
  rc = fchdir( start->dd_handle );
#elif defined FREEBSD
  rc = fchdir( dirfd( start ) );
#elif defined OPENBSD
  rc = fchdir( dirfd( start ) );
#else
  rc = fchdir( start->dd_fd );
#endif

  closedir( start );
  return rc;
}

/****
 *
 * display output
 *
 ****/

int display( int level, const char *format, ... ) {
  PRIVATE va_list args;
  PRIVATE char tmp_buf[SYSLOG_MAX];
  PRIVATE int i;
  PRIVATE int ret;

  if ( format == NULL ) {
    return FAILED;
  }

  va_start( args, format );
  ret = vsnprintf( tmp_buf, sizeof(tmp_buf), format, args );
  if ( ret >= 0 && ret < sizeof(tmp_buf) && tmp_buf[strlen(tmp_buf)-1] == '\n' ) {
    tmp_buf[strlen(tmp_buf)-1] = 0;
  } else if ( ret >= sizeof(tmp_buf) ) {
    /* truncated, ensure null termination */
    tmp_buf[sizeof(tmp_buf)-1] = 0;
  }
  va_end( args );

  if ( config->mode != MODE_INTERACTIVE ) {
    /* display info via syslog */
    syslog( level, "%s", tmp_buf );
  } else {
    if ( level <= LOG_ERR ) {
      /* display info via stderr */
      for ( i = 0; prioritynames[i].c_name != NULL; i++ ) {
        if ( prioritynames[i].c_val == level ) {
          fprintf( stderr, "%s[%u] - %s\n", prioritynames[i].c_name, config->cur_pid, tmp_buf );
          return TRUE;
        }
      }
    } else {
      /* display info via stdout */
      for ( i = 0; prioritynames[i].c_name != NULL; i++ ) {
        if ( prioritynames[i].c_val == level ) {
          printf( "%s[%u] - %s\n", prioritynames[i].c_name, config->cur_pid, tmp_buf );
          return TRUE;
        }
      }
    }
  }

  return FAILED;
}

/****
 *
 * safely open a file for writing
 *
 ****/

static int safe_open( const char *filename ) {
  int fd;
  struct stat sb;
  XMEMSET( &sb, 0, sizeof( struct stat ) );
                                                                 
  if ( lstat(filename, &sb) EQ FAILED ) {
    if (errno != ENOENT)
      return( FAILED );
  } else if ( ( sb.st_mode & S_IFREG) EQ 0 ) {
    errno = EOPNOTSUPP;
    return ( FAILED );
  }

  unlink( filename );
#ifdef MINGW
  fd = open( filename, O_WRONLY|O_CREAT|O_EXCL, S_IRUSR|S_IWUSR );
#else
  fd = open( filename, O_WRONLY|O_CREAT|O_EXCL, S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH );
#endif

  return (fd);
}

/****
 *
 * create pid file
 *
 ****/

int create_pid_file( const char *filename ) {
    int fd;
    char buf[32];
    struct flock fl;
     
#ifdef O_CLOEXEC
    fd = open( filename, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR | O_CLOEXEC );
#else
    fd = open( filename, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR );
#endif    
    if ( fd EQ FAILED ) {
        display( LOG_ERR, "Unable to open PID file [%s]", filename );
        return( EXIT_FAILURE );
    }

#ifndef O_CLOEXEC
    /* use fcntl if O_CLOEXEC not supported */
    flags = fcntl( fd, F_GETFD );
    if ( flags EQ FAILED ) {
        display( LOG_ERR, "Unable to get flags" );
        return( EXIT_FAILURE );
    }
    flags |= FD_CLOEXEC;
    if ( fcntl(fd, F_SETFD, flags ) EQ FAILED ) {
        display( LOG_ERR, "Unable to set flags" );
        return( EXIT_FAILURE );
    }
#endif
 
    /* set a lock on the file */
    fl.l_type = F_WRLCK;
    fl.l_whence = SEEK_SET;
    fl.l_start = 0;
    fl.l_len = 0;
    if ( fcntl(fd, F_SETLK, &fl) EQ FAILED ) {
       if ( errno EQ EAGAIN || errno == EACCES )
           display( LOG_ERR, "Process is already running" );
       else
           display( LOG_ERR, "Unable to lock PID file [%s]", filename );
       return( EXIT_FAILURE );
    }

    /* write the PID to the file */
    if (ftruncate( fd, 0 ) EQ -FAILED ) {
        display( LOG_ERR, "Unable to empty PID file [%s]", filename );
        return( EXIT_FAILURE );
    }
    
    snprintf( buf, sizeof( buf ), "%ld\n", (long)getpid() );
    if ( write( fd, buf, strlen(buf) ) != strlen( buf ) ) {
        display( LOG_ERR, "Unable to write PID to file [%s]", filename );
        return( EXIT_FAILURE );
    }
 
    return fd;
}


/****
 *
 * cleaup pid file
 *
 ****/

void cleanup_pid_file( const char *filename ) {
  if ( strlen( filename ) > 0 ) {
    unlink( filename );
  }
}

/****
 *
 * sanitize environment
 *
 ****/

void sanitize_environment( void ) {
  int i;
  char **new_environ;
  char *ptr, *value, *var;
  size_t arr_size = 1;
  size_t arr_ptr = 0;
  size_t len;
  size_t new_size = 0;

  for( i = 0; (var = restricted_environ[i]) != 0; i++ ) {
    new_size += strlen( var ) + 1;
    arr_size++;
  }

  for ( i = 0; (var = preserve_environ[i]) != 0; i++ ) {
    if ( !(value = getenv(var))) continue;
    new_size += strlen( var ) + strlen( value ) + 2;
    arr_size++;
  }

  new_size += ( arr_size * sizeof( char * ) );
  new_environ = (char **)XMALLOC( new_size );
  new_environ[arr_size - 1] = 0;
  ptr = ( char * )new_environ + (arr_size * sizeof(char *));
  for ( i = 0; ( var = restricted_environ[i] ) != 0; i++ ) {
    new_environ[arr_ptr++] = ptr;
    len = strlen( var );
    XMEMCPY( ptr, var, len + 1 );
    ptr += len + 1;
  }

  for ( i = 0; ( var = preserve_environ[i] ) != 0; i++ ) {
    if ( !( value = getenv( var ) ) ) continue;
    new_environ[arr_ptr++] = ptr;
    len = strlen( var );
    XMEMCPY( ptr, var, len );
    *(ptr + len + 1 ) = '=';
    XMEMCPY( ptr + len + 2, value, strlen( value ) + 1 );
    ptr += len + strlen( value ) + 2;
  }

  environ = new_environ;
}

/****
 *
 * is it an odd number
 *
 ****/

inline int isodd(const int n) {
  return n % 2;
}

/****
 *
 * is it an even number
 *
 ****/

inline int iseven(const int n) {
  return !isodd(n);
}
