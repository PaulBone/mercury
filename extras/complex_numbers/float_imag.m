%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2001, 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: float_imag.m.
% Main author: fjh.
% Stability: medium.

% This module provides binary operators on (float, imag).
%
% See also: complex.m, imag.m, float.m, imag_float.m.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module complex_numbers.float_imag.
:- interface.

:- import_module complex_numbers.complex.
:- import_module complex_numbers.imag.

:- import_module float.

%-----------------------------------------------------------------------------%

    % Addition.
    % 
:- func float + imag = complex.
:- mode in    + in   = uo  is det.

    % Subtraction.
    %
:- func float - imag = complex.
:- mode in    - in   = uo  is det.

    % Multiplication.
    %
:- func float * imag = imag.
:- mode in    * in   = uo  is det.

    % Division.
    %
:- func float / imag = imag.
:- mode in    / in   = uo  is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

XR + im(YI) = cmplx(0.0 + XR, 0.0 + YI).
XR - im(YI) = cmplx(0.0 + XR, 0.0 - YI).
XR * im(YI) = im(XR * YI).
XR / im(YI) = im(0.0 - XR / YI).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
