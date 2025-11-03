/*************************************************************************

    File: presupTestSuite.pl
    Copyright (C) 2004,2006 Patrick Blackburn & Johan Bos

    This file is part of BB2, version 2.0 (November 2006).

    BB2 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB2 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB2; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
	
	
	editiert: eigene Testsätze

*************************************************************************/

:- module(presupTestSuite,[discourse/2]).


/*========================================================================
    Example Discourses
========================================================================*/
discourse([a,dragon,sleeps,in,a,cave,the,dragon,dreams],2).

discourse([the,dragon,sleeps],1).

discourse([the,dragon,does,not,sleep],1).

discourse([a,dragon,sleeps,in,a,cave,it,dreams],1).

discourse([it,sleeps],0).

discourse([if,a,viking,meets,a,dragon,he,tames,the,dragon],2).

discourse([if,a,viking,meets,a,dragon,he,tames,it],1).

discourse([every,viking,who,meets,a,dragon,tames,the,dragon],2).

discourse([a,viking,meets,a,big,dragon,and,a,small,dragon,the,dragon,roars],0).

