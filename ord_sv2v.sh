#**************************************************************************)
#*                                                                        *)
#* OCaml template Copyright (C) 2004-2010                                 *)
#*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
#* Adapted to boolean logic by Jonathan Kimmitt                           *)
#*  Copyright 2016 University of Cambridge                                *)
#*                                                                        *)
#*  This software is free software; you can redistribute it and/or        *)
#*  modify it under the terms of the GNU Library General Public           *)
#*  License version 2.1, with the special exception on linking            *)
#*  described in file LICENSE.                                            *)
#*                                                                        *)
#*  This software is distributed in the hope that it will be useful,      *)
#*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
#*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
#*                                                                        *)
#**************************************************************************)

echo open String > sv2v_ord.ml
echo open Sv2v >> sv2v_ord.ml
echo let getstr = function >> sv2v_ord.ml
sed '/exception/,$d' sv2v.mli | grep '\ [A-Z][A-Za-z_]' | cut -d\( -f1 | tr '\011' ' ' |\
sed -e 's=[|\ ]*\([A-Z][A-Za-z0-9_\ o]*\)=|\ \1 -> uppercase_ascii(\"\1\") \;=' -e 's= of [A-Za-z0-9\ ]*= _=' -e 's= of[A-Za-z0-9\ ]*==' | cut -d\; -f1 | sort >> sv2v_ord.ml
