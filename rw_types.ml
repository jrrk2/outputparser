open Printf

type txt = {txt: string}

type ident =
| Lident of string

type ident' = {txt: ident}

type ptyp = {ptyp_desc: tuple; ptyp_loc_stack: ptyp list}

and tuple =
| Pcstr_tuple of ptyp list
| Ptyp_constr of ident' * ptyp list

type pcd = {pcd_name: txt; pcd_args: tuple; pcd_res: unit option}

let lst = 
        [{pcd_name = {txt = "Active"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "vtyp"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Add"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "AlwaysComb"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "AlwaysComb2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "AlwaysFF"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "AlwaysLatch"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "AlwaysLegacy"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "AlwaysSync"}; pcd_args = Pcstr_tuple [];
          pcd_res = None};
         {pcd_name = {txt = "And"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "And2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "And3"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "AnyRange"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Asgn1"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "AsgnPat"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Assert"}; pcd_args = Pcstr_tuple [];
          pcd_res = None};
         {pcd_name = {txt = "AssertProperty"}; pcd_args = Pcstr_tuple [];
          pcd_res = None};
         {pcd_name = {txt = "At"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "AtStar"}; pcd_args = Pcstr_tuple [];
          pcd_res = None};
         {pcd_name = {txt = "Atom"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "AutoFunDecl"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "BeginBlock"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Bitlst"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "BlockItem"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Blocking"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "BreakSemi"}; pcd_args = Pcstr_tuple [];
          pcd_res = None};
         {pcd_name = {txt = "CaretTilde"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "CaseItm"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "CaseStart"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "CaseStart1"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "CaseStartInside"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "CaseStartUniq"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "CaseStartUniqInside"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "CaseStmt"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Cast"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "CellParamItem2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "CellParamItem3"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "CellPinItem2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "CellPinItemImplied"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "CellPinItemNC"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Concat"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "CondGen1"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "ContAsgn"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "DeclAsgn"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "DeclData"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "DeclInt2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "DeclLogic"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "DeclLogic2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "DeclModPort"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "DeclReg"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Deflt"}; pcd_args = Pcstr_tuple [];
          pcd_res = None};
         {pcd_name = {txt = "Div"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Dot1"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Dot3"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "DotBus"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Edge"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "ElabTask"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "ElseStmt"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "EnumInit"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Equals"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Equals3"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "EqualsQuery"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Equate"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "EquateArrayField"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "EquateConcat"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "EquateField"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "EquateSelect"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "EquateSelect2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "EquateSlice"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "EquateSlicePlus"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "EventOr"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "ExprOKL"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "ExprQuote1"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Expression"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Final"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Float"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "float"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgn"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgn1"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnArrayField"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnArrayField2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnArrayField3"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnArrayField4"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnArrayField5"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnArrayField6"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnArrayField7"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnArrayField8"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnArrayField9"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnArrayMemSel"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnArrayRange"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnArrayRange2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnArraySel"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnArrayWid"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FopAsgnConcat"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "ForEach"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "ForLoop"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FunDecl"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FunGuts"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FunRef"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "FunRef2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "GenBlock"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "GenItem"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Genvar"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Greater"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "GtEq"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Hash"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "HyphenGt"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Id"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "IdArrayed1"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "IdArrayed2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "IdArrayed3"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "IdArrayedColon"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "IdArrayedHyphenColon"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "IdArrayedPlusColon"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "If1"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "If2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Iff"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Import"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "In"}; pcd_args = Pcstr_tuple []; pcd_res = None};
         {pcd_name = {txt = "Inc"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "InitPair"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "InitPat"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "InitSig"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Initial"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Inout"}; pcd_args = Pcstr_tuple [];
          pcd_res = None};
         {pcd_name = {txt = "InsideCase"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "InsideRange"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "InstArrayDecl"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "InstDecl"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "InstNameParen1"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "InstNameParen2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "InstRange"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "IntfDecl"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Intgr"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "int"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "ItemAsgn"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Itmlst"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Less"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "LocalParamTyp"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Logic"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "LoopGen1"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "LtEq"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "LtGt"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Mod"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "ModPortItm"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Modul"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Mult"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Nand"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Neg"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "NetDecl"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "NonBlocking"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Nor"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "NotEq"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "NotEq3"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "NotEqQuery"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Number"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "int"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "int"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "int"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "OpenRange"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Or"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Or2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Out"}; pcd_args = Pcstr_tuple [];
          pcd_res = None};
         {pcd_name = {txt = "PackageBody"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "PackageParam"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "PackageParam2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "PackageRef"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Param"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "ParamAsgn1"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "ParamAsgn2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "ParamDecl"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "ParamPort"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "PatMember1"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "PatMemberDflt"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "PkgImport"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "PkgImportItm"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Pling"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Port"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "PortDir"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "PortFront"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "PortItem"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "PortItemFront"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "PortItemFront2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "PortsStar"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Pos"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "PropertySpec"}; pcd_args = Pcstr_tuple [];
          pcd_res = None};
         {pcd_name = {txt = "Query"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "RedAnd"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "RedOr"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "RedXor"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Repl"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Return"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Split"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "SUDecl"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "SUMember"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Seq"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Shiftl"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Shiftr"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Shiftr3"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "SideEffect"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Signed"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "StarStar"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "String"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Sub"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Sys"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "SysFuncCall"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "SysTaskCall"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "SysTaskRef"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "TFBody"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "TF_port_decl"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "TF_variable"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "TaskDecl"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "TaskRef"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "TaskRef2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Tilde"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "TildeAnd"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "TildeOr"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Typ1"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Typ2"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Typ3"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Typ4"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Typ5"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Typ6"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Typ7"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Typ8"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Typ9"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Typ10"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Typ11"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Typ12"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "TypEnum3"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "TypEnum4"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "TypEnum5"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "TypEnum6"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "TypParam"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "UMinus"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "UPlus"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Unimplemented"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Union"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Unknown"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Unsigned"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "VNum"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "string"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "ValueRange"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "VarDeclAsgn"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "VarDim"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "While"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc =
               Ptyp_constr ({txt = Lident "list"},
                [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
                  ptyp_loc_stack = []}]);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "WireExpr"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Xnor"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None};
         {pcd_name = {txt = "Xor"};
          pcd_args =
           Pcstr_tuple
            [{ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []};
             {ptyp_desc = Ptyp_constr ({txt = Lident "rw"}, []);
              ptyp_loc_stack = []}];
          pcd_res = None}]

let unhand = ref None
let unhand' = ref None

let dumplst = function
| {ptyp_desc = Ptyp_constr ({txt = Lident id}, []); ptyp_loc_stack = []} -> id
| {ptyp_desc = Ptyp_constr ({txt = Lident lst},
          [{ptyp_desc = Ptyp_constr ({txt = Lident id}, []);
            ptyp_loc_stack = []}]);
        ptyp_loc_stack = []} -> id ^ " " ^ lst
| oth -> unhand' := Some oth; failwith "dumplst"

let do_descend = function
| "string" -> false
| "float" -> false
| "int" -> false
| oth -> true

let dumplst' rhs ix = function
| {ptyp_desc = Ptyp_constr ({txt = Lident id}, []); ptyp_loc_stack = []} -> (if rhs && do_descend id then "descend_itm attr " else "") ^ id ^ string_of_int (ix+1)
| {ptyp_desc = Ptyp_constr ({txt = Lident lst},
          [{ptyp_desc = Ptyp_constr ({txt = Lident id}, []);
            ptyp_loc_stack = []}]);
        ptyp_loc_stack = []} -> (if rhs then "descend_lst attr " else "") ^ id ^ "_lst" ^ string_of_int (ix+1)
| oth -> unhand' := Some oth; failwith "dumplst'"

let dump' fd = List.iter (function
| {pcd_name = {txt = nam}; pcd_args = Pcstr_tuple lst;
 pcd_res = None} -> if lst <> [] then fprintf fd "  | %s of %s\n" nam (String.concat " * " (List.map dumplst lst)) else fprintf fd "  | %s\n" nam
| oth -> unhand := Some oth; failwith "rw_type") (List.sort compare lst)

let dump'' fd = List.iter (function
| {pcd_name = {txt = nam}; pcd_args = Pcstr_tuple lst;
 pcd_res = None} -> 
  if lst <> [] then
    begin
      fprintf fd "  | %s(%s) -> " nam (String.concat ", " (List.mapi (dumplst' false) lst));
      fprintf fd "%s(%s)\n" nam (String.concat ", " (List.mapi (dumplst' true) lst));
    end
  else
     fprintf fd "  | %s -> %s\n" nam nam
| oth -> unhand := Some oth; failwith "rw_type") (List.sort compare lst)

let dumplst3 rhs prime ix = function
| {ptyp_desc = Ptyp_constr ({txt = Lident id}, []); ptyp_loc_stack = []} ->
  (if do_descend id then
     (if rhs then id ^ string_of_int (ix+1) ^ (if prime then "'" else "")
      else
        "Split(" ^ id ^ string_of_int (ix+1) ^ ", " ^ id ^ string_of_int (ix+1) ^ "')")
      else id ^ string_of_int (ix+1))

| {ptyp_desc = Ptyp_constr ({txt = Lident lst},
          [{ptyp_desc = Ptyp_constr ({txt = Lident id}, []);
            ptyp_loc_stack = []}]);
        ptyp_loc_stack = []} -> (if rhs then (if prime then "snd(" else "fst(") ^ "split_pair_lst " else "") ^ id ^ "_lst" ^ string_of_int (ix+1) ^ (if rhs then ")" else "")
| oth -> unhand' := Some oth; failwith "dumplst'"

let dump3 fd = List.iter (function
| {pcd_name = {txt = "Active"}} -> ()
| {pcd_name = {txt = nam}; pcd_args = Pcstr_tuple lst; pcd_res = None} -> 
  if lst <> [] then
    begin
      fprintf fd "  | %s(%s) -> " nam (String.concat ", " (List.mapi (dumplst3 false false) lst));
      fprintf fd "%s(%s), " nam (String.concat ", " (List.mapi (dumplst3 true false) lst));
      fprintf fd "%s(%s)\n" nam (String.concat ", " (List.mapi (dumplst3 true true) lst));
    end
  else
     fprintf fd "  | %s -> %s,%s\n" nam nam nam
| oth -> unhand := Some oth; failwith "rw_type") (List.sort compare lst)

let dump = 
  let fd = open_out "Source_text_rewrite_types_new.mli" in
  fprintf fd "type mem_opts = {off:int list; siz:int list; wid:int; tot:int}\n";
  fprintf fd "type rw = \n";
  dump' fd;
  fprintf fd "and vtyp =\n";
  fprintf fd "  | Vint of int\n";
  fprintf fd "  | Vpkg of string * string\n";
  fprintf fd "  | Unsigned\n";
  fprintf fd "  | Unsigned_vector of rw * rw\n";
  fprintf fd "  | Signed\n";
  fprintf fd "  | Signed_vector of rw * rw\n";
  fprintf fd "  | Vsigtyp\n";
  fprintf fd "  | Vdot\n";
  fprintf fd "  | Vstr of string\n";
  fprintf fd "  | Vtyp of string\n";
  fprintf fd "  | Vfun of string\n";
  fprintf fd "  | Venum of string\n";
  fprintf fd "  | Vintf of rw\n";
  fprintf fd "  | MaybePort of int * vtyp * rw\n";
  fprintf fd "  | Vemember of string * string * rw\n";
  fprintf fd "  | Task of rw * rw * rw\n";
  fprintf fd "  | Vmem of mem_opts\n";
  fprintf fd "  | InstArray of rw * rw * rw\n";
  fprintf fd "  | Vlong of int64\n";
  fprintf fd "  | Vreal of float\n";
  fprintf fd "  | Vlocal of int * rw\n";
  fprintf fd "  | Vsu of rw * (string * vtyp) list\n";
  fprintf fd "  | Vsua of int * int * (string * vtyp) list\n";
  fprintf fd "\n";
  fprintf fd "module E = Msat_sat_slit.String_lit (* expressions *)\n";
  fprintf fd "module F = Msat_tseitin.MakeCNF\n";
  fprintf fd "\n";
  fprintf fd "type ind = {\n";
  fprintf fd "  wires:(E.signal, F.t option) Hashtbl.t;\n";
  fprintf fd "  inffop:(E.signal, unit) Hashtbl.t;\n";
  fprintf fd "  stash:(E.signal, string * string * Input_rewrite_types.ilang list) Hashtbl.t;\n";
  fprintf fd "  wid:(string, int) Hashtbl.t;\n";
  fprintf fd "}\n";
  fprintf fd "\n";
  fprintf fd "type dead =\n";
  fprintf fd "| Undecidable\n";
  fprintf fd "| Always_false\n";
  fprintf fd "| Always_true\n";
  close_out fd;
  let fd = open_out "Source_text_rewrite_new.ml" in
  fprintf fd "let rec descend' (attr:attr) = function\n";
  dump'' fd;
  close_out fd;
  let fd = open_out "Source_text_split_new.ml" in
  fprintf fd "open Source_text_rewrite_types\n\n";
  fprintf fd "let unhand_split_lst = ref []\n";
  fprintf fd "let unhand_split = ref (Itmlst [])\n\n";
  fprintf fd "let rec split_pair = function\n";
  dump3 fd;
  fprintf fd "| oth -> unhand_split := oth; failwith \"split_pair\"\n";
  fprintf fd "\n";
  fprintf fd "and split_pair_lst = function\n";
  fprintf fd "| [] -> [], []\n";
  fprintf fd "| Split (p,q) :: tl -> let r, s = split_pair_lst tl in p :: r, q :: s\n";
  fprintf fd "| oth -> unhand_split_lst := oth; failwith \"split_pair_lst\"\n";
  fprintf fd "\n";
  close_out fd
