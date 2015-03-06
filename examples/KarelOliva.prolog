:- op(1200, xfx, is_a_rule_if) .
:- op(1200, xf, is_a_rule) .

% PARSE(
%   "Frozen"_Goal_Cat,
%   [Real_Goal_Cat,Structure],
%   Input_String,Rest_String )

/* Checking whether parsing the current Real Goal Category
from some prefix of the Input String has been
tried (either successfully or not) in the preceding
steps of the parsing process. */
parse(Goal_Category,[Real_Goal_Category,Structure],
      Input_String, Rest_String)
:-
    ( already_parsed(Asserted_Goal_Category,_,
                     Input_String,_ ),
      identical_categories(Goal_Category,
                           Asserted_Goal_Category) ;
      cannot_be_parsed(Asserted_Goal_Category,
                       Input_String ),
      identical_categories{Goal_Category,
                           Asserted_Goal_Category),
      !, fail ),
!,
already_parsed(Asserted_Goal_Category,
               [Real_Goal_Category, Structure],
               Input_String, Rest_String ),
identical_categorles(Goal_Category,
                     Asserted_Goal_Category) .

/* The following clause describes parsing of a category
with no daughters (category immediately dominating
an empty string) */
parse(Goal_Category,[Real_Goal_Category,d_trs=[]],
      String, String )
:-
/* rule having no daughters is to be found in the grammar */
    find_rule_to_be_used(Real_Goal_Category,
                         Constraints_Of_Rule),
    call(Constraints_Of_Rule),
    assertz(already_parsed(
                    Goal_Category,
                    [Real_Goal_Category,d_trs=[]],
                    String, String )) .

/* The following clause describes parsing of a category
dominating a non-empty terminal string */
parse(Goal_Category,[Real_Goal_Category,Structure],
      [Word_Form|Rest_Input_Strlng],Rest_String )
:-
    lexicon(Word_Form,Word_Form_Category),
    perestroika(Word_Form_Category,
                Real_Word_Form_Category),
    is_a_left_corner(
             [Real_Word_Form_Category, d_trs=[]],
             [Real_Goal_Category,Structure],
             Rest_Input_String, Rest_String ),
    assertz(already_parsed(
                    Goal_Category,
                    [Real_Goal_Category,Structure],
                    [Word_Form|Rest_Input_String],
                    Rest_String )) .

/* Asserting information about the impossibility of
parsing certain categories from certain strings */
parse(Goal_Category,_,InputStrlng,_)
:-
    (already_parsed(Goal_Category,_,Input_String,_) ;
     assertz(cannot_be_parsed(
                     Goal_Category,Input_String)) ),
! , fail .

% IS_A_LEFT_CORNER(
%   [Real_Left_Corner_Cat,Structure],
%   [Real_Goal_Cat,Structure],
%   Input_String,Rest_String )

/* reflexive closure of the relation "being a left corner" */
is_a_left_corner(
        [Real_Left_Corner_Category,
         Real_Left_Corner_Category_Structure],
        [Real_Goal_Category,Real_Goal_Category_Structure],
        String,String )
:-
    unify(Real_Left_Corner_Category,
          Real_Goal_Category ) .

/* transitive closure of the relation "being a left corner" */
is_a_left_corner(
        [Real_Left_Corner_Category,
         Real_Left_Corner_Category_Structure],
        [Real_Goal_Category,Real_Goal_Category_Structure],
        Input_String, Rest_String )
:-

    /* a rule having the current left corner category as its left corner is to be found */
    find_rule_to_be_used(
            Real_Left_Corner_Category,
            Left_Daughter_Marking,
            Right_Sisters_List_From_Current_Rule,
            Mother_Category_From_Current_Rule,
            Constraints_Of_Rule,
            Type_Of_Rule ),

    /* all the right sisters have to be parsed */
    parse_right_sisters(
            Right_Sisters_List_From_Current_Rule,
            Real_Right_Sisters_List,
            Input_String, Intermediary_String,
            Type_Of_Rule ),

    call(Constraints_Of_Rule),

    /* the mother category from the rule must comply
with the feature inheritance principles relevant for
the Type Of Rule */
    feature_inheritance_princlples_concerning_mother(
            [Real_Left_Corner_Category
                 |Real_Right_Sisters_List],
            Mother_Category_From_Current_Rule,
            Real_Mother_Category,
            Type_Of_Rule ),

/* the mother itself (?herself?) is used as a left
corner, which repeats the process on a higher level */
    is_a_left_corner(
            [RealMother_Category,
             d_trs=[Left_Daughter_Marking =
                    [Real_Left_Corner_Category,
                     Real_Left_Corner_Category_Structure],
                    |Real_Right_Sisters_List ]],
            [Real_Goal_Category,Real_Goal_Category_Structure],
            Intermediary_String,Rest_Strlng ).

/* The whole parsing process is started by asking the conjunction of goals */
?- parse(TOPMOST_CATEGORY, Intermediary_Result, INPUT, []),
   perestroika(RESULT, Intermediary_Result) .
