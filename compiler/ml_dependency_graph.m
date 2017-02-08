%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_dependency_graph.m.
% Main authors: pbone
%
% This dependency graph records which mlds definitions depend on which other
% definitions.  It is a specialisation of the one in dependency_graph.m.
%
%-----------------------------------------------------------------------------%
:- module ml_backend.ml_dependency_graph.
:- interface.

:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module ml_backend.mlds.

%-----------------------------------------------------------------------------%

:- type ml_dependency_info      == dependency_info(mlds_entity_name).

:- type ml_dependency_ordering  == dependency_ordering(mlds_entity_name).
:- type ml_dependency_graph     == dependency_graph(mlds_entity_name).
:- type ml_dependency_graph_key == dependency_graph_key(mlds_entity_name).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
