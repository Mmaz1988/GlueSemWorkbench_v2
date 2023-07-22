/*************************************************************************

    File: errors.pl
    
    Copyright (C) 2020 Johan Bos (johan.bos@rug.nl)

    This file is part of Boxer, version October 2020.

    This component is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.

    This file is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this module; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
    02111-1307 USA
	
*************************************************************************/

:- module(errors,[warning/2,error/2]).

warning(S,V):-
   format(user_error,'\033[33mWARNING: ',[]),
   format(user_error,S,V),
   format(user_error,'\033[0m~n',[]).

error(S,V):-
   format(user_error,'\033[31mERROR: ',[]),
   format(user_error,S,V),
   format(user_error,'\033[0m~n',[]).

