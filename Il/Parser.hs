module Il.Parser where

import Il.Lexer
import Defs.AST
import Prelude hiding (True, False)

-- parser produced by Happy Version 1.18.2

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Term)
	| HappyAbsSyn8 ([Term])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (13) = happyShift action_7
action_0 (19) = happyShift action_8
action_0 (22) = happyShift action_9
action_0 (23) = happyShift action_10
action_0 (25) = happyShift action_11
action_0 (31) = happyShift action_12
action_0 (42) = happyShift action_13
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (7) = happyGoto action_5
action_0 (8) = happyGoto action_6
action_0 _ = happyFail

action_1 (31) = happyShift action_2
action_1 _ = happyFail

action_2 (11) = happyShift action_36
action_2 _ = happyFail

action_3 (32) = happyShift action_35
action_3 _ = happyReduce_42

action_4 _ = happyReduce_10

action_5 (13) = happyShift action_7
action_5 (19) = happyShift action_8
action_5 (22) = happyShift action_9
action_5 (23) = happyShift action_10
action_5 (25) = happyShift action_11
action_5 (31) = happyShift action_12
action_5 (42) = happyShift action_13
action_5 (4) = happyGoto action_3
action_5 (5) = happyGoto action_4
action_5 (7) = happyGoto action_5
action_5 (8) = happyGoto action_34
action_5 _ = happyReduce_3

action_6 (43) = happyAccept
action_6 _ = happyFail

action_7 (31) = happyShift action_33
action_7 _ = happyFail

action_8 (28) = happyShift action_32
action_8 _ = happyFail

action_9 (13) = happyShift action_7
action_9 (18) = happyShift action_25
action_9 (23) = happyShift action_10
action_9 (24) = happyShift action_26
action_9 (28) = happyShift action_27
action_9 (31) = happyShift action_28
action_9 (33) = happyShift action_29
action_9 (34) = happyShift action_30
action_9 (41) = happyShift action_31
action_9 (5) = happyGoto action_23
action_9 (6) = happyGoto action_24
action_9 _ = happyFail

action_10 (31) = happyShift action_22
action_10 _ = happyFail

action_11 (13) = happyShift action_7
action_11 (19) = happyShift action_8
action_11 (22) = happyShift action_9
action_11 (23) = happyShift action_10
action_11 (25) = happyShift action_11
action_11 (31) = happyShift action_12
action_11 (32) = happyShift action_20
action_11 (37) = happyShift action_21
action_11 (42) = happyShift action_13
action_11 (4) = happyGoto action_3
action_11 (5) = happyGoto action_4
action_11 (7) = happyGoto action_5
action_11 (8) = happyGoto action_19
action_11 _ = happyFail

action_12 (11) = happyShift action_15
action_12 (13) = happyShift action_16
action_12 (23) = happyShift action_17
action_12 (28) = happyShift action_18
action_12 _ = happyFail

action_13 (28) = happyShift action_14
action_13 _ = happyFail

action_14 (13) = happyShift action_7
action_14 (18) = happyShift action_25
action_14 (23) = happyShift action_10
action_14 (24) = happyShift action_26
action_14 (28) = happyShift action_27
action_14 (31) = happyShift action_28
action_14 (33) = happyShift action_29
action_14 (34) = happyShift action_30
action_14 (41) = happyShift action_31
action_14 (5) = happyGoto action_23
action_14 (6) = happyGoto action_62
action_14 _ = happyFail

action_15 (13) = happyShift action_7
action_15 (15) = happyShift action_61
action_15 (18) = happyShift action_25
action_15 (23) = happyShift action_10
action_15 (24) = happyShift action_26
action_15 (28) = happyShift action_27
action_15 (31) = happyShift action_28
action_15 (33) = happyShift action_29
action_15 (34) = happyShift action_30
action_15 (41) = happyShift action_31
action_15 (5) = happyGoto action_23
action_15 (6) = happyGoto action_37
action_15 _ = happyFail

action_16 _ = happyReduce_14

action_17 _ = happyReduce_12

action_18 (13) = happyShift action_7
action_18 (18) = happyShift action_25
action_18 (23) = happyShift action_10
action_18 (24) = happyShift action_26
action_18 (28) = happyShift action_27
action_18 (31) = happyShift action_28
action_18 (33) = happyShift action_29
action_18 (34) = happyShift action_30
action_18 (41) = happyShift action_31
action_18 (5) = happyGoto action_23
action_18 (6) = happyGoto action_59
action_18 (9) = happyGoto action_60
action_18 _ = happyReduce_45

action_19 (37) = happyShift action_58
action_19 _ = happyFail

action_20 (13) = happyShift action_7
action_20 (19) = happyShift action_8
action_20 (22) = happyShift action_9
action_20 (23) = happyShift action_10
action_20 (25) = happyShift action_11
action_20 (31) = happyShift action_12
action_20 (37) = happyShift action_57
action_20 (42) = happyShift action_13
action_20 (4) = happyGoto action_3
action_20 (5) = happyGoto action_4
action_20 (7) = happyGoto action_5
action_20 (8) = happyGoto action_56
action_20 _ = happyFail

action_21 _ = happyReduce_37

action_22 _ = happyReduce_11

action_23 _ = happyReduce_34

action_24 (10) = happyShift action_43
action_24 (14) = happyShift action_44
action_24 (17) = happyShift action_45
action_24 (20) = happyShift action_46
action_24 (21) = happyShift action_47
action_24 (26) = happyShift action_48
action_24 (27) = happyShift action_49
action_24 (29) = happyShift action_50
action_24 (30) = happyShift action_51
action_24 (32) = happyShift action_52
action_24 (35) = happyShift action_53
action_24 (36) = happyShift action_54
action_24 (40) = happyShift action_55
action_24 _ = happyFail

action_25 _ = happyReduce_19

action_26 _ = happyReduce_18

action_27 (13) = happyShift action_7
action_27 (18) = happyShift action_25
action_27 (23) = happyShift action_10
action_27 (24) = happyShift action_26
action_27 (28) = happyShift action_27
action_27 (31) = happyShift action_28
action_27 (33) = happyShift action_29
action_27 (34) = happyShift action_30
action_27 (41) = happyShift action_31
action_27 (5) = happyGoto action_23
action_27 (6) = happyGoto action_42
action_27 _ = happyFail

action_28 (13) = happyShift action_16
action_28 (23) = happyShift action_17
action_28 (28) = happyShift action_18
action_28 _ = happyReduce_16

action_29 (13) = happyShift action_7
action_29 (18) = happyShift action_25
action_29 (23) = happyShift action_10
action_29 (24) = happyShift action_26
action_29 (28) = happyShift action_27
action_29 (31) = happyShift action_28
action_29 (33) = happyShift action_29
action_29 (34) = happyShift action_30
action_29 (41) = happyShift action_31
action_29 (5) = happyGoto action_23
action_29 (6) = happyGoto action_41
action_29 _ = happyFail

action_30 _ = happyReduce_17

action_31 _ = happyReduce_20

action_32 (13) = happyShift action_7
action_32 (19) = happyShift action_8
action_32 (22) = happyShift action_9
action_32 (23) = happyShift action_10
action_32 (25) = happyShift action_11
action_32 (31) = happyShift action_12
action_32 (42) = happyShift action_13
action_32 (4) = happyGoto action_39
action_32 (5) = happyGoto action_4
action_32 (7) = happyGoto action_40
action_32 _ = happyFail

action_33 _ = happyReduce_13

action_34 _ = happyReduce_39

action_35 (13) = happyShift action_7
action_35 (19) = happyShift action_8
action_35 (22) = happyShift action_9
action_35 (23) = happyShift action_10
action_35 (25) = happyShift action_11
action_35 (31) = happyShift action_12
action_35 (42) = happyShift action_13
action_35 (4) = happyGoto action_3
action_35 (5) = happyGoto action_4
action_35 (7) = happyGoto action_5
action_35 (8) = happyGoto action_38
action_35 _ = happyReduce_41

action_36 (13) = happyShift action_7
action_36 (18) = happyShift action_25
action_36 (23) = happyShift action_10
action_36 (24) = happyShift action_26
action_36 (28) = happyShift action_27
action_36 (31) = happyShift action_28
action_36 (33) = happyShift action_29
action_36 (34) = happyShift action_30
action_36 (41) = happyShift action_31
action_36 (5) = happyGoto action_23
action_36 (6) = happyGoto action_37
action_36 _ = happyFail

action_37 (10) = happyShift action_43
action_37 (14) = happyShift action_44
action_37 (17) = happyShift action_45
action_37 (20) = happyShift action_46
action_37 (21) = happyShift action_47
action_37 (26) = happyShift action_48
action_37 (27) = happyShift action_49
action_37 (29) = happyShift action_50
action_37 (30) = happyShift action_51
action_37 (35) = happyShift action_53
action_37 (36) = happyShift action_54
action_37 _ = happyReduce_1

action_38 _ = happyReduce_40

action_39 (39) = happyShift action_81
action_39 _ = happyFail

action_40 _ = happyReduce_3

action_41 (10) = happyShift action_43
action_41 (14) = happyShift action_44
action_41 (17) = happyShift action_45
action_41 (20) = happyShift action_46
action_41 (21) = happyShift action_47
action_41 (26) = happyShift action_48
action_41 (27) = happyShift action_49
action_41 (29) = happyShift action_50
action_41 (30) = happyShift action_51
action_41 (35) = happyShift action_53
action_41 (36) = happyShift action_54
action_41 _ = happyReduce_21

action_42 (10) = happyShift action_43
action_42 (14) = happyShift action_44
action_42 (17) = happyShift action_45
action_42 (20) = happyShift action_46
action_42 (21) = happyShift action_47
action_42 (26) = happyShift action_48
action_42 (27) = happyShift action_49
action_42 (29) = happyShift action_50
action_42 (30) = happyShift action_51
action_42 (35) = happyShift action_53
action_42 (36) = happyShift action_54
action_42 (38) = happyShift action_80
action_42 _ = happyFail

action_43 (13) = happyShift action_7
action_43 (18) = happyShift action_25
action_43 (23) = happyShift action_10
action_43 (24) = happyShift action_26
action_43 (28) = happyShift action_27
action_43 (31) = happyShift action_28
action_43 (33) = happyShift action_29
action_43 (34) = happyShift action_30
action_43 (41) = happyShift action_31
action_43 (5) = happyGoto action_23
action_43 (6) = happyGoto action_79
action_43 _ = happyFail

action_44 (13) = happyShift action_7
action_44 (18) = happyShift action_25
action_44 (23) = happyShift action_10
action_44 (24) = happyShift action_26
action_44 (28) = happyShift action_27
action_44 (31) = happyShift action_28
action_44 (33) = happyShift action_29
action_44 (34) = happyShift action_30
action_44 (41) = happyShift action_31
action_44 (5) = happyGoto action_23
action_44 (6) = happyGoto action_78
action_44 _ = happyFail

action_45 (13) = happyShift action_7
action_45 (18) = happyShift action_25
action_45 (23) = happyShift action_10
action_45 (24) = happyShift action_26
action_45 (28) = happyShift action_27
action_45 (31) = happyShift action_28
action_45 (33) = happyShift action_29
action_45 (34) = happyShift action_30
action_45 (41) = happyShift action_31
action_45 (5) = happyGoto action_23
action_45 (6) = happyGoto action_77
action_45 _ = happyFail

action_46 (13) = happyShift action_7
action_46 (18) = happyShift action_25
action_46 (23) = happyShift action_10
action_46 (24) = happyShift action_26
action_46 (28) = happyShift action_27
action_46 (31) = happyShift action_28
action_46 (33) = happyShift action_29
action_46 (34) = happyShift action_30
action_46 (41) = happyShift action_31
action_46 (5) = happyGoto action_23
action_46 (6) = happyGoto action_76
action_46 _ = happyFail

action_47 (13) = happyShift action_7
action_47 (18) = happyShift action_25
action_47 (23) = happyShift action_10
action_47 (24) = happyShift action_26
action_47 (28) = happyShift action_27
action_47 (31) = happyShift action_28
action_47 (33) = happyShift action_29
action_47 (34) = happyShift action_30
action_47 (41) = happyShift action_31
action_47 (5) = happyGoto action_23
action_47 (6) = happyGoto action_75
action_47 _ = happyFail

action_48 (13) = happyShift action_7
action_48 (18) = happyShift action_25
action_48 (23) = happyShift action_10
action_48 (24) = happyShift action_26
action_48 (28) = happyShift action_27
action_48 (31) = happyShift action_28
action_48 (33) = happyShift action_29
action_48 (34) = happyShift action_30
action_48 (41) = happyShift action_31
action_48 (5) = happyGoto action_23
action_48 (6) = happyGoto action_74
action_48 _ = happyFail

action_49 (13) = happyShift action_7
action_49 (18) = happyShift action_25
action_49 (23) = happyShift action_10
action_49 (24) = happyShift action_26
action_49 (28) = happyShift action_27
action_49 (31) = happyShift action_28
action_49 (33) = happyShift action_29
action_49 (34) = happyShift action_30
action_49 (41) = happyShift action_31
action_49 (5) = happyGoto action_23
action_49 (6) = happyGoto action_73
action_49 _ = happyFail

action_50 (13) = happyShift action_7
action_50 (18) = happyShift action_25
action_50 (23) = happyShift action_10
action_50 (24) = happyShift action_26
action_50 (28) = happyShift action_27
action_50 (31) = happyShift action_28
action_50 (33) = happyShift action_29
action_50 (34) = happyShift action_30
action_50 (41) = happyShift action_31
action_50 (5) = happyGoto action_23
action_50 (6) = happyGoto action_72
action_50 _ = happyFail

action_51 (13) = happyShift action_7
action_51 (18) = happyShift action_25
action_51 (23) = happyShift action_10
action_51 (24) = happyShift action_26
action_51 (28) = happyShift action_27
action_51 (31) = happyShift action_28
action_51 (33) = happyShift action_29
action_51 (34) = happyShift action_30
action_51 (41) = happyShift action_31
action_51 (5) = happyGoto action_23
action_51 (6) = happyGoto action_71
action_51 _ = happyFail

action_52 (40) = happyShift action_70
action_52 _ = happyFail

action_53 (13) = happyShift action_7
action_53 (18) = happyShift action_25
action_53 (23) = happyShift action_10
action_53 (24) = happyShift action_26
action_53 (28) = happyShift action_27
action_53 (31) = happyShift action_28
action_53 (33) = happyShift action_29
action_53 (34) = happyShift action_30
action_53 (41) = happyShift action_31
action_53 (5) = happyGoto action_23
action_53 (6) = happyGoto action_69
action_53 _ = happyFail

action_54 (13) = happyShift action_7
action_54 (18) = happyShift action_25
action_54 (23) = happyShift action_10
action_54 (24) = happyShift action_26
action_54 (28) = happyShift action_27
action_54 (31) = happyShift action_28
action_54 (33) = happyShift action_29
action_54 (34) = happyShift action_30
action_54 (41) = happyShift action_31
action_54 (5) = happyGoto action_23
action_54 (6) = happyGoto action_68
action_54 _ = happyFail

action_55 (13) = happyShift action_7
action_55 (19) = happyShift action_8
action_55 (22) = happyShift action_9
action_55 (23) = happyShift action_10
action_55 (25) = happyShift action_11
action_55 (31) = happyShift action_12
action_55 (42) = happyShift action_13
action_55 (4) = happyGoto action_67
action_55 (5) = happyGoto action_4
action_55 (7) = happyGoto action_40
action_55 _ = happyFail

action_56 (37) = happyShift action_66
action_56 _ = happyFail

action_57 _ = happyReduce_38

action_58 _ = happyReduce_36

action_59 (10) = happyShift action_43
action_59 (12) = happyShift action_65
action_59 (14) = happyShift action_44
action_59 (17) = happyShift action_45
action_59 (20) = happyShift action_46
action_59 (21) = happyShift action_47
action_59 (26) = happyShift action_48
action_59 (27) = happyShift action_49
action_59 (29) = happyShift action_50
action_59 (30) = happyShift action_51
action_59 (35) = happyShift action_53
action_59 (36) = happyShift action_54
action_59 _ = happyReduce_44

action_60 (38) = happyShift action_64
action_60 _ = happyFail

action_61 _ = happyReduce_2

action_62 (10) = happyShift action_43
action_62 (14) = happyShift action_44
action_62 (17) = happyShift action_45
action_62 (20) = happyShift action_46
action_62 (21) = happyShift action_47
action_62 (26) = happyShift action_48
action_62 (27) = happyShift action_49
action_62 (29) = happyShift action_50
action_62 (30) = happyShift action_51
action_62 (35) = happyShift action_53
action_62 (36) = happyShift action_54
action_62 (38) = happyShift action_63
action_62 _ = happyFail

action_63 (13) = happyShift action_7
action_63 (19) = happyShift action_8
action_63 (22) = happyShift action_9
action_63 (23) = happyShift action_10
action_63 (25) = happyShift action_11
action_63 (31) = happyShift action_12
action_63 (32) = happyShift action_87
action_63 (42) = happyShift action_13
action_63 (4) = happyGoto action_86
action_63 (5) = happyGoto action_4
action_63 (7) = happyGoto action_40
action_63 _ = happyFail

action_64 _ = happyReduce_15

action_65 (13) = happyShift action_7
action_65 (18) = happyShift action_25
action_65 (23) = happyShift action_10
action_65 (24) = happyShift action_26
action_65 (28) = happyShift action_27
action_65 (31) = happyShift action_28
action_65 (33) = happyShift action_29
action_65 (34) = happyShift action_30
action_65 (41) = happyShift action_31
action_65 (5) = happyGoto action_23
action_65 (6) = happyGoto action_59
action_65 (9) = happyGoto action_85
action_65 _ = happyReduce_45

action_66 _ = happyReduce_35

action_67 (16) = happyShift action_84
action_67 _ = happyFail

action_68 (14) = happyShift action_44
action_68 (30) = happyShift action_51
action_68 _ = happyReduce_24

action_69 (14) = happyShift action_44
action_69 (17) = happyShift action_45
action_69 (20) = happyShift action_46
action_69 (21) = happyShift action_47
action_69 (26) = happyShift action_48
action_69 (27) = happyShift action_49
action_69 (29) = happyShift action_50
action_69 (30) = happyShift action_51
action_69 (36) = happyShift action_54
action_69 _ = happyReduce_23

action_70 (13) = happyShift action_7
action_70 (19) = happyShift action_8
action_70 (22) = happyShift action_9
action_70 (23) = happyShift action_10
action_70 (25) = happyShift action_11
action_70 (31) = happyShift action_12
action_70 (42) = happyShift action_13
action_70 (4) = happyGoto action_83
action_70 (5) = happyGoto action_4
action_70 (7) = happyGoto action_40
action_70 _ = happyFail

action_71 _ = happyReduce_26

action_72 (14) = happyShift action_44
action_72 (30) = happyShift action_51
action_72 _ = happyReduce_25

action_73 (14) = happyShift action_44
action_73 (17) = happyFail
action_73 (20) = happyFail
action_73 (21) = happyFail
action_73 (26) = happyFail
action_73 (27) = happyFail
action_73 (29) = happyShift action_50
action_73 (30) = happyShift action_51
action_73 (36) = happyShift action_54
action_73 _ = happyReduce_32

action_74 (14) = happyShift action_44
action_74 (17) = happyFail
action_74 (20) = happyFail
action_74 (21) = happyFail
action_74 (26) = happyFail
action_74 (27) = happyFail
action_74 (29) = happyShift action_50
action_74 (30) = happyShift action_51
action_74 (36) = happyShift action_54
action_74 _ = happyReduce_29

action_75 (14) = happyShift action_44
action_75 (17) = happyFail
action_75 (20) = happyFail
action_75 (21) = happyFail
action_75 (26) = happyFail
action_75 (27) = happyFail
action_75 (29) = happyShift action_50
action_75 (30) = happyShift action_51
action_75 (36) = happyShift action_54
action_75 _ = happyReduce_31

action_76 (14) = happyShift action_44
action_76 (17) = happyFail
action_76 (20) = happyFail
action_76 (21) = happyFail
action_76 (26) = happyFail
action_76 (27) = happyFail
action_76 (29) = happyShift action_50
action_76 (30) = happyShift action_51
action_76 (36) = happyShift action_54
action_76 _ = happyReduce_30

action_77 (14) = happyShift action_44
action_77 (17) = happyFail
action_77 (20) = happyFail
action_77 (21) = happyFail
action_77 (26) = happyFail
action_77 (27) = happyFail
action_77 (29) = happyShift action_50
action_77 (30) = happyShift action_51
action_77 (36) = happyShift action_54
action_77 _ = happyReduce_28

action_78 _ = happyReduce_27

action_79 (14) = happyShift action_44
action_79 (17) = happyShift action_45
action_79 (20) = happyShift action_46
action_79 (21) = happyShift action_47
action_79 (26) = happyShift action_48
action_79 (27) = happyShift action_49
action_79 (29) = happyShift action_50
action_79 (30) = happyShift action_51
action_79 (36) = happyShift action_54
action_79 _ = happyReduce_22

action_80 _ = happyReduce_33

action_81 (13) = happyShift action_7
action_81 (18) = happyShift action_25
action_81 (23) = happyShift action_10
action_81 (24) = happyShift action_26
action_81 (28) = happyShift action_27
action_81 (31) = happyShift action_28
action_81 (33) = happyShift action_29
action_81 (34) = happyShift action_30
action_81 (41) = happyShift action_31
action_81 (5) = happyGoto action_23
action_81 (6) = happyGoto action_82
action_81 _ = happyFail

action_82 (10) = happyShift action_43
action_82 (14) = happyShift action_44
action_82 (17) = happyShift action_45
action_82 (20) = happyShift action_46
action_82 (21) = happyShift action_47
action_82 (26) = happyShift action_48
action_82 (27) = happyShift action_49
action_82 (29) = happyShift action_50
action_82 (30) = happyShift action_51
action_82 (35) = happyShift action_53
action_82 (36) = happyShift action_54
action_82 (39) = happyShift action_91
action_82 _ = happyFail

action_83 (32) = happyShift action_90
action_83 _ = happyFail

action_84 (13) = happyShift action_7
action_84 (19) = happyShift action_8
action_84 (22) = happyShift action_9
action_84 (23) = happyShift action_10
action_84 (25) = happyShift action_11
action_84 (31) = happyShift action_12
action_84 (42) = happyShift action_13
action_84 (4) = happyGoto action_89
action_84 (5) = happyGoto action_4
action_84 (7) = happyGoto action_40
action_84 _ = happyFail

action_85 _ = happyReduce_43

action_86 _ = happyReduce_8

action_87 (13) = happyShift action_7
action_87 (19) = happyShift action_8
action_87 (22) = happyShift action_9
action_87 (23) = happyShift action_10
action_87 (25) = happyShift action_11
action_87 (31) = happyShift action_12
action_87 (42) = happyShift action_13
action_87 (4) = happyGoto action_88
action_87 (5) = happyGoto action_4
action_87 (7) = happyGoto action_40
action_87 _ = happyFail

action_88 _ = happyReduce_9

action_89 _ = happyReduce_4

action_90 (16) = happyShift action_93
action_90 _ = happyFail

action_91 (13) = happyShift action_7
action_91 (19) = happyShift action_8
action_91 (22) = happyShift action_9
action_91 (23) = happyShift action_10
action_91 (25) = happyShift action_11
action_91 (31) = happyShift action_12
action_91 (42) = happyShift action_13
action_91 (4) = happyGoto action_92
action_91 (5) = happyGoto action_4
action_91 (7) = happyGoto action_40
action_91 _ = happyFail

action_92 (38) = happyShift action_95
action_92 _ = happyFail

action_93 (13) = happyShift action_7
action_93 (19) = happyShift action_8
action_93 (22) = happyShift action_9
action_93 (23) = happyShift action_10
action_93 (25) = happyShift action_11
action_93 (31) = happyShift action_12
action_93 (42) = happyShift action_13
action_93 (4) = happyGoto action_94
action_93 (5) = happyGoto action_4
action_93 (7) = happyGoto action_40
action_93 _ = happyFail

action_94 _ = happyReduce_5

action_95 (13) = happyShift action_7
action_95 (19) = happyShift action_8
action_95 (22) = happyShift action_9
action_95 (23) = happyShift action_10
action_95 (25) = happyShift action_11
action_95 (31) = happyShift action_12
action_95 (32) = happyShift action_97
action_95 (42) = happyShift action_13
action_95 (4) = happyGoto action_96
action_95 (5) = happyGoto action_4
action_95 (7) = happyGoto action_40
action_95 _ = happyFail

action_96 _ = happyReduce_6

action_97 (13) = happyShift action_7
action_97 (19) = happyShift action_8
action_97 (22) = happyShift action_9
action_97 (23) = happyShift action_10
action_97 (25) = happyShift action_11
action_97 (31) = happyShift action_12
action_97 (42) = happyShift action_13
action_97 (4) = happyGoto action_98
action_97 (5) = happyGoto action_4
action_97 (7) = happyGoto action_40
action_97 _ = happyFail

action_98 _ = happyReduce_7

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal ((_,TkName happy_var_1)))
	 =  HappyAbsSyn4
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 _
	_
	(HappyTerminal ((_,TkName happy_var_1)))
	 =  HappyAbsSyn4
		 (DSInit happy_var_1
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyReduce 6 4 happyReduction_4
happyReduction_4 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 8 4 happyReduction_5
happyReduction_5 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 9 4 happyReduction_6
happyReduction_6 ((HappyAbsSyn4  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (While happy_var_5 (Block [happy_var_3, happy_var_7, happy_var_9])
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 10 4 happyReduction_7
happyReduction_7 ((HappyAbsSyn4  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (While happy_var_5 (Block [happy_var_3, happy_var_7, happy_var_10])
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 5 4 happyReduction_8
happyReduction_8 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (While happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 6 4 happyReduction_9
happyReduction_9 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (While happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  5 happyReduction_11
happyReduction_11 (HappyTerminal ((_,TkName happy_var_2)))
	_
	 =  HappyAbsSyn4
		 (Inc happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  5 happyReduction_12
happyReduction_12 _
	(HappyTerminal ((_,TkName happy_var_1)))
	 =  HappyAbsSyn4
		 (Inc happy_var_1
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  5 happyReduction_13
happyReduction_13 (HappyTerminal ((_,TkName happy_var_2)))
	_
	 =  HappyAbsSyn4
		 (Dec happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  5 happyReduction_14
happyReduction_14 _
	(HappyTerminal ((_,TkName happy_var_1)))
	 =  HappyAbsSyn4
		 (Dec happy_var_1
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 5 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((_,TkName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Funcall happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  6 happyReduction_16
happyReduction_16 (HappyTerminal ((_,TkName happy_var_1)))
	 =  HappyAbsSyn4
		 (Var happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  6 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn4
		 (Int 0
	)

happyReduce_18 = happySpecReduce_1  6 happyReduction_18
happyReduction_18 (HappyTerminal ((_,TkInt happy_var_1)))
	 =  HappyAbsSyn4
		 (Int happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  6 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn4
		 (Int 0
	)

happyReduce_20 = happySpecReduce_1  6 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn4
		 (Int 1
	)

happyReduce_21 = happySpecReduce_2  6 happyReduction_21
happyReduction_21 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Not happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  6 happyReduction_22
happyReduction_22 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  6 happyReduction_23
happyReduction_23 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  6 happyReduction_24
happyReduction_24 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Sum happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  6 happyReduction_25
happyReduction_25 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  6 happyReduction_26
happyReduction_26 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  6 happyReduction_27
happyReduction_27 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Div happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  6 happyReduction_28
happyReduction_28 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  6 happyReduction_29
happyReduction_29 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Leq happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  6 happyReduction_30
happyReduction_30 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Geq happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  6 happyReduction_31
happyReduction_31 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Gt happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  6 happyReduction_32
happyReduction_32 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Lt happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  6 happyReduction_33
happyReduction_33 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  6 happyReduction_34
happyReduction_34 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happyReduce 4 7 happyReduction_35
happyReduction_35 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Block happy_var_3
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_3  7 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Block happy_var_2
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  7 happyReduction_37
happyReduction_37 _
	_
	 =  HappyAbsSyn4
		 (Block []
	)

happyReduce_38 = happySpecReduce_3  7 happyReduction_38
happyReduction_38 _
	_
	_
	 =  HappyAbsSyn4
		 (Block []
	)

happyReduce_39 = happySpecReduce_2  8 happyReduction_39
happyReduction_39 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1:happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  8 happyReduction_40
happyReduction_40 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1:happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  8 happyReduction_41
happyReduction_41 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  8 happyReduction_42
happyReduction_42 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  9 happyReduction_43
happyReduction_43 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1:happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  9 happyReduction_44
happyReduction_44 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_0  9 happyReduction_45
happyReduction_45  =  HappyAbsSyn8
		 ([]
	)

happyNewToken action sts stk [] =
	action 43 43 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	(_,TkAnd) -> cont 10;
	(_,TkAssign) -> cont 11;
	(_,TkComma) -> cont 12;
	(_,TkDec) -> cont 13;
	(_,TkDiv) -> cont 14;
	(_,TkDS) -> cont 15;
	(_,TkElse) -> cont 16;
	(_,TkEquals) -> cont 17;
	(_,TkFalse) -> cont 18;
	(_,TkFor) -> cont 19;
	(_,TkGEqual) -> cont 20;
	(_,TkGreater) -> cont 21;
	(_,TkIf) -> cont 22;
	(_,TkInc) -> cont 23;
	(_,TkInt happy_dollar_dollar) -> cont 24;
	(_,TkLCParen) -> cont 25;
	(_,TkLEqual) -> cont 26;
	(_,TkLess) -> cont 27;
	(_,TkLParen) -> cont 28;
	(_,TkMinus) -> cont 29;
	(_,TkMul) -> cont 30;
	(_,TkName happy_dollar_dollar) -> cont 31;
	(_,TkNewline) -> cont 32;
	(_,TkNot) -> cont 33;
	(_,TkNull) -> cont 34;
	(_,TkOr) -> cont 35;
	(_,TkPlus) -> cont 36;
	(_,TkRCParen) -> cont 37;
	(_,TkRParen) -> cont 38;
	(_,TkSemicolon) -> cont 39;
	(_,TkThen) -> cont 40;
	(_,TkTrue) -> cont 41;
	(_,TkWhile) -> cont 42;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError (((line,col),t):xs) = error $ "Parse error at line " ++ (show line) ++ ", column " ++ (show col)
parseError [] = error "Parse error at the end"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}








{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
