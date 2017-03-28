namespace MsharP3

//Auto generated tables
module HuffmanTables = 
    //Quad Table ((code,codesize),values)
    let quadTable = [
        ((1,1),[0; 0; 0; 0]);((5,4),[0; 0; 0; 1]);((4,4),[0; 0; 1; 0]);((5,5),[0; 0; 1; 1]); 
        ((6,4),[0; 1; 0; 0]);((5,6),[0; 1; 0; 1]);((4,5),[0; 1; 1; 0]);((4,6),[0; 1; 1; 1]); 
        ((7,4),[1; 0; 0; 0]);((3,5),[1; 0; 0; 1]);((6,5),[1; 0; 1; 0]);((0,6),[1; 0; 1; 1]);
        ((7,5),[1; 1; 0; 0]);((2,6),[1; 1; 0; 1]);((3,6),[1; 1; 1; 0]);((1,6),[1; 1; 1; 1]);
    ]

    //Huffman tables
    let table0 = [
        [(0,1);];
    ]

    let table1 = [
        [(1,1);(1,3);];
        [(1,2);(0,3);];
    ]

    let table2 = [
        [(1,1);(2,3);(1,6);];
        [(3,3);(1,3);(1,5);];
        [(3,5);(2,5);(0,6);];
    ]

    let table3 = [
        [(3,2);(2,2);(1,6);];
        [(1,3);(1,2);(1,5);];
        [(3,5);(2,5);(0,6);];
    ]

    let table5 = [
        [(1,1);(2,3);(6,6);(5,7);];
        [(3,3);(1,3);(4,6);(4,7);];
        [(7,6);(5,6);(7,7);(1,8);];
        [(6,7);(1,6);(1,7);(0,8);];
    ]

    let table6 = [
        [(7,3);(3,3);(5,5);(1,7);(6,3);(2,2);];
        [(6,3);(2,2);(3,4);(2,5);(5,4);(4,4);];
        [(5,4);(4,4);(4,5);(1,6);(3,6);(3,5);];
        [(3,6);(3,5);(2,6);(0,7);(1,1);(2,3);];
        [(1,1);(2,3);(10,6);(19,8);(16,8);(10,9);];
        [(16,8);(10,9);(3,3);(3,4);(7,6);(10,7);];
    ]

    let table7 = [
        [(1,1);(2,3);(10,6);(19,8);(16,8);(10,9);];
        [(3,3);(3,4);(7,6);(10,7);(5,7);(3,8);];
        [(11,6);(4,5);(13,7);(17,8);(8,8);(4,9);];
        [(12,7);(11,7);(18,8);(15,9);(11,9);(2,9);];
        [(7,7);(6,7);(9,8);(14,9);(3,9);(1,10);];
        [(6,8);(4,8);(5,9);(3,10);(2,10);(0,10);];
    ]

    let table8 = [
        [(3,2);(4,3);(6,6);(18,8);(12,8);(5,9);];
        [(5,3);(1,2);(2,4);(16,8);(9,8);(3,8);];
        [(7,6);(3,4);(5,6);(14,8);(7,8);(3,9);];
        [(19,8);(17,8);(15,8);(13,9);(10,9);(4,10);];
        [(13,8);(5,7);(8,8);(11,9);(5,10);(1,10);];
        [(12,9);(4,8);(4,9);(1,9);(1,11);(0,11);];
    ]

    let table9 = [
        [(7,3);(5,3);(9,5);(14,6);(15,8);(7,9);];
        [(6,3);(4,3);(5,4);(5,5);(6,6);(7,8);];
        [(7,4);(6,4);(8,5);(8,6);(8,7);(5,8);];
        [(15,6);(6,5);(9,6);(10,7);(5,7);(1,8);];
        [(11,7);(7,6);(9,7);(6,7);(4,8);(1,9);];
        [(14,8);(4,7);(6,8);(2,8);(6,9);(0,9);];
    ]

    let table10 = [
        [(1,1);(2,3);(10,6);(23,8);(35,9);(30,9);];
        [(3,3);(3,4);(8,6);(12,7);(18,8);(21,9);];
        [(11,6);(9,6);(15,7);(21,8);(32,9);(40,10);];
        [(14,7);(13,7);(22,8);(34,9);(46,10);(23,10);];
        [(20,8);(19,8);(33,9);(47,10);(27,10);(22,10);];
        [(31,9);(22,9);(41,10);(26,10);(21,11);(20,11);];
    ]

    let table11 = [
        [(3,2);(4,3);(10,5);(24,7);(34,8);(33,9);(21,8);(15,9);];
        [(5,3);(3,3);(4,4);(10,6);(32,8);(17,8);(11,7);(10,8);];
        [(11,5);(7,5);(13,6);(18,7);(30,8);(31,9);(20,8);(5,8);];
        [(25,7);(11,6);(19,7);(59,9);(27,8);(18,10);(12,8);(5,9);];
        [(35,8);(33,8);(31,8);(58,9);(30,9);(16,10);(7,9);(5,10);];
        [(28,8);(26,8);(32,9);(19,10);(17,10);(15,11);(8,10);(14,11);];
        [(14,8);(12,7);(9,7);(13,8);(14,9);(9,10);(4,10);(1,10);];
        [(11,8);(4,7);(6,8);(6,9);(6,10);(3,10);(2,10);(0,10);];
    ]

    let table12 = [
        [(9,4);(6,3);(16,5);(33,7);(41,8);(39,9);(38,9);(26,9);];
        [(7,3);(5,3);(6,4);(9,5);(23,7);(16,7);(26,8);(11,8);];
        [(17,5);(7,4);(11,5);(14,6);(21,7);(30,8);(10,7);(7,8);];
        [(17,6);(10,5);(15,6);(12,6);(18,7);(28,8);(14,8);(5,8);];
        [(32,7);(13,6);(22,7);(19,7);(18,8);(16,8);(9,8);(5,9);];
        [(40,8);(17,7);(31,8);(29,8);(17,8);(13,9);(4,8);(2,9);];
        [(27,8);(12,7);(11,7);(15,8);(10,8);(7,9);(4,9);(1,10);];
        [(27,9);(12,8);(8,8);(12,9);(6,9);(3,9);(1,9);(0,10);];
    ]

    let table13 = [
        [(1,1);(5,4);(14,6);(21,7);(34,8);(51,9);(46,9);(71,10);(42,9);(52,10);(68,11);(52,11);(67,12);(44,12);(43,13);(19,13);];
        [(3,3);(4,4);(12,6);(19,7);(31,8);(26,8);(44,9);(33,9);(31,9);(24,9);(32,10);(24,10);(31,11);(35,12);(22,12);(14,12);];
        [(15,6);(13,6);(23,7);(36,8);(59,9);(49,9);(77,10);(65,10);(29,9);(40,10);(30,10);(40,11);(27,11);(33,12);(42,13);(16,13);];
        [(22,7);(20,7);(37,8);(61,9);(56,9);(79,10);(73,10);(64,10);(43,10);(76,11);(56,11);(37,11);(26,11);(31,12);(25,13);(14,13);];
        [(35,8);(16,7);(60,9);(57,9);(97,10);(75,10);(114,11);(91,11);(54,10);(73,11);(55,11);(41,12);(48,12);(53,13);(23,13);(24,14);];
        [(58,9);(27,8);(50,9);(96,10);(76,10);(70,10);(93,11);(84,11);(77,11);(58,11);(79,12);(29,11);(74,13);(49,13);(41,14);(17,14);];
        [(47,9);(45,9);(78,10);(74,10);(115,11);(94,11);(90,11);(79,11);(69,11);(83,12);(71,12);(50,12);(59,13);(38,13);(36,14);(15,14);];
        [(72,10);(34,9);(56,10);(95,11);(92,11);(85,11);(91,12);(90,12);(86,12);(73,12);(77,13);(65,13);(51,13);(44,14);(43,16);(42,16);];
        [(43,9);(20,8);(30,9);(44,10);(55,10);(78,11);(72,11);(87,12);(78,12);(61,12);(46,12);(54,13);(37,13);(30,14);(20,15);(16,15);];
        [(53,10);(25,9);(41,10);(37,10);(44,11);(59,11);(54,11);(81,13);(66,12);(76,13);(57,13);(54,14);(37,14);(18,14);(39,16);(11,15);];
        [(35,10);(33,10);(31,10);(57,11);(42,11);(82,12);(72,12);(80,13);(47,12);(58,13);(55,14);(21,13);(22,14);(26,15);(38,16);(22,17);];
        [(53,11);(25,10);(23,10);(38,11);(70,12);(60,12);(51,12);(36,12);(55,13);(26,13);(34,13);(23,14);(27,15);(14,15);(9,15);(7,16);];
        [(34,11);(32,11);(28,11);(39,12);(49,12);(75,13);(30,12);(52,13);(48,14);(40,14);(52,15);(28,15);(18,15);(17,16);(9,16);(5,16);];
        [(45,12);(21,11);(34,12);(64,13);(56,13);(50,13);(49,14);(45,14);(31,14);(19,14);(12,14);(15,15);(10,16);(7,15);(6,16);(3,16);];
        [(48,13);(23,12);(20,12);(39,13);(36,13);(35,13);(53,15);(21,14);(16,14);(23,17);(13,15);(10,15);(6,15);(1,17);(4,16);(2,16);];
        [(16,12);(15,12);(17,13);(27,14);(25,14);(20,14);(29,15);(11,14);(17,15);(12,15);(16,16);(8,16);(1,19);(1,18);(0,19);(1,16);];
    ]

    let table15 = [
        [(7,3);(12,4);(18,5);(53,7);(47,7);(76,8);(124,9);(108,9);(89,9);(123,10);(108,10);(119,11);(107,11);(81,11);(122,12);(63,13);];
        [(13,4);(5,3);(16,5);(27,6);(46,7);(36,7);(61,8);(51,8);(42,8);(70,9);(52,9);(83,10);(65,10);(41,10);(59,11);(36,11);];
        [(19,5);(17,5);(15,5);(24,6);(41,7);(34,7);(59,8);(48,8);(40,8);(64,9);(50,9);(78,10);(62,10);(80,11);(56,11);(33,11);];
        [(29,6);(28,6);(25,6);(43,7);(39,7);(63,8);(55,8);(93,9);(76,9);(59,9);(93,10);(72,10);(54,10);(75,11);(50,11);(29,11);];
        [(52,7);(22,6);(42,7);(40,7);(67,8);(57,8);(95,9);(79,9);(72,9);(57,9);(89,10);(69,10);(49,10);(66,11);(46,11);(27,11);];
        [(77,8);(37,7);(35,7);(66,8);(58,8);(52,8);(91,9);(74,9);(62,9);(48,9);(79,10);(63,10);(90,11);(62,11);(40,11);(38,12);];
        [(125,9);(32,7);(60,8);(56,8);(50,8);(92,9);(78,9);(65,9);(55,9);(87,10);(71,10);(51,10);(73,11);(51,11);(70,12);(30,12);];
        [(109,9);(53,8);(49,8);(94,9);(88,9);(75,9);(66,9);(122,10);(91,10);(73,10);(56,10);(42,10);(64,11);(44,11);(21,11);(25,12);];
        [(90,9);(43,8);(41,8);(77,9);(73,9);(63,9);(56,9);(92,10);(77,10);(66,10);(47,10);(67,11);(48,11);(53,12);(36,12);(20,12);];
        [(71,9);(34,8);(67,9);(60,9);(58,9);(49,9);(88,10);(76,10);(67,10);(106,11);(71,11);(54,11);(38,11);(39,12);(23,12);(15,12);];
        [(109,10);(53,9);(51,9);(47,9);(90,10);(82,10);(58,10);(57,10);(48,10);(72,11);(57,11);(41,11);(23,11);(27,12);(62,13);(9,12);];
        [(86,10);(42,9);(40,9);(37,9);(70,10);(64,10);(52,10);(43,10);(70,11);(55,11);(42,11);(25,11);(29,12);(18,12);(11,12);(11,13);];
        [(118,11);(68,10);(30,9);(55,10);(50,10);(46,10);(74,11);(65,11);(49,11);(39,11);(24,11);(16,11);(22,12);(13,12);(14,13);(7,13);];
        [(91,11);(44,10);(39,10);(38,10);(34,10);(63,11);(52,11);(45,11);(31,11);(52,12);(28,12);(19,12);(14,12);(8,12);(9,13);(3,13);];
        [(123,12);(60,11);(58,11);(53,11);(47,11);(43,11);(32,11);(22,11);(37,12);(24,12);(17,12);(12,12);(15,13);(10,13);(2,12);(1,13);];
        [(71,12);(37,11);(34,11);(30,11);(28,11);(20,11);(17,11);(26,12);(21,12);(16,12);(10,12);(6,12);(8,13);(6,13);(2,13);(0,13);];
    ]

    let table16 = [
        [(1,1);(5,4);(14,6);(44,8);(74,9);(63,9);(110,10);(93,10);(172,11);(149,11);(138,11);(242,12);(225,12);(195,12);(376,13);(17,9);];
        [(3,3);(4,4);(12,6);(20,7);(35,8);(62,9);(53,9);(47,9);(83,10);(75,10);(68,10);(119,11);(201,12);(107,11);(207,12);(9,8);];
        [(15,6);(13,6);(23,7);(38,8);(67,9);(58,9);(103,10);(90,10);(161,11);(72,10);(127,11);(117,11);(110,11);(209,12);(206,12);(16,9);];
        [(45,8);(21,7);(39,8);(69,9);(64,9);(114,10);(99,10);(87,10);(158,11);(140,11);(252,12);(212,12);(199,12);(387,13);(365,13);(26,10);];
        [(75,9);(36,8);(68,9);(65,9);(115,10);(101,10);(179,11);(164,11);(155,11);(264,12);(246,12);(226,12);(395,13);(382,13);(362,13);(9,9);];
        [(66,9);(30,8);(59,9);(56,9);(102,10);(185,11);(173,11);(265,12);(142,11);(253,12);(232,12);(400,13);(388,13);(378,13);(445,14);(16,10);];
        [(111,10);(54,9);(52,9);(100,10);(184,11);(178,11);(160,11);(133,11);(257,12);(244,12);(228,12);(217,12);(385,13);(366,13);(715,14);(10,10);];
        [(98,10);(48,9);(91,10);(88,10);(165,11);(157,11);(148,11);(261,12);(248,12);(407,13);(397,13);(372,13);(380,13);(889,15);(884,15);(8,10);];
        [(85,10);(84,10);(81,10);(159,11);(156,11);(143,11);(260,12);(249,12);(427,13);(401,13);(392,13);(383,13);(727,14);(713,14);(708,14);(7,10);];
        [(154,11);(76,10);(73,10);(141,11);(131,11);(256,12);(245,12);(426,13);(406,13);(394,13);(384,13);(735,14);(359,13);(710,14);(352,13);(11,11);];
        [(139,11);(129,11);(67,10);(125,11);(247,12);(233,12);(229,12);(219,12);(393,13);(743,14);(737,14);(720,14);(885,15);(882,15);(439,14);(4,10);];
        [(243,12);(120,11);(118,11);(115,11);(227,12);(223,12);(396,13);(746,14);(742,14);(736,14);(721,14);(712,14);(706,14);(223,13);(436,14);(6,11);];
        [(202,12);(224,12);(222,12);(218,12);(216,12);(389,13);(386,13);(381,13);(364,13);(888,15);(443,14);(707,14);(440,14);(437,14);(1728,16);(4,11);];
        [(747,14);(211,12);(210,12);(208,12);(370,13);(379,13);(734,14);(723,14);(714,14);(1735,16);(883,15);(877,15);(876,15);(3459,17);(865,15);(2,11);];
        [(377,13);(369,13);(102,11);(187,12);(726,14);(722,14);(358,13);(711,14);(709,14);(866,15);(1734,16);(871,15);(3458,17);(870,15);(434,14);(0,11);];
        [(12,9);(10,8);(7,8);(11,9);(10,9);(17,10);(11,10);(9,10);(13,11);(12,11);(10,11);(7,11);(5,11);(3,11);(1,11);(3,8);];
    ]

    let table24 = [
        [(15,4);(13,4);(46,6);(80,7);(146,8);(262,9);(248,9);(434,10);(426,10);(669,11);(653,11);(649,11);(621,11);(517,11);(1032,12);(88,9);];
        [(14,4);(12,4);(21,5);(38,6);(71,7);(130,8);(122,8);(216,9);(209,9);(198,9);(327,10);(345,10);(319,10);(297,10);(279,10);(42,8);];
        [(47,6);(22,5);(41,6);(74,7);(68,7);(128,8);(120,8);(221,9);(207,9);(194,9);(182,9);(340,10);(315,10);(295,10);(541,11);(18,7);];
        [(81,7);(39,6);(75,7);(70,7);(134,8);(125,8);(116,8);(220,9);(204,9);(190,9);(178,9);(325,10);(311,10);(293,10);(271,10);(16,7);];
        [(147,8);(72,7);(69,7);(135,8);(127,8);(118,8);(112,8);(210,9);(200,9);(188,9);(352,10);(323,10);(306,10);(285,10);(540,11);(14,7);];
        [(263,9);(66,7);(129,8);(126,8);(119,8);(114,8);(214,9);(202,9);(192,9);(180,9);(341,10);(317,10);(301,10);(281,10);(262,10);(12,7);];
        [(249,9);(123,8);(121,8);(117,8);(113,8);(215,9);(206,9);(195,9);(185,9);(347,10);(330,10);(308,10);(291,10);(272,10);(520,11);(10,7);];
        [(435,10);(115,8);(111,8);(109,8);(211,9);(203,9);(196,9);(187,9);(353,10);(332,10);(313,10);(298,10);(283,10);(531,11);(381,11);(17,8);];
        [(427,10);(212,9);(208,9);(205,9);(201,9);(193,9);(186,9);(177,9);(169,9);(320,10);(303,10);(286,10);(268,10);(514,11);(377,11);(16,8);];
        [(335,10);(199,9);(197,9);(191,9);(189,9);(181,9);(174,9);(333,10);(321,10);(305,10);(289,10);(275,10);(521,11);(379,11);(371,11);(11,8);];
        [(668,11);(184,9);(183,9);(179,9);(175,9);(344,10);(331,10);(314,10);(304,10);(290,10);(277,10);(530,11);(383,11);(373,11);(366,11);(10,8);];
        [(652,11);(346,10);(171,9);(168,9);(164,9);(318,10);(309,10);(299,10);(287,10);(276,10);(263,10);(513,11);(375,11);(368,11);(362,11);(6,8);];
        [(648,11);(322,10);(316,10);(312,10);(307,10);(302,10);(292,10);(284,10);(269,10);(261,10);(512,11);(376,11);(370,11);(364,11);(359,11);(4,8);];
        [(620,11);(300,10);(296,10);(294,10);(288,10);(282,10);(273,10);(266,10);(515,11);(380,11);(374,11);(369,11);(365,11);(361,11);(357,11);(2,8);];
        [(1033,12);(280,10);(278,10);(274,10);(267,10);(264,10);(259,10);(382,11);(378,11);(372,11);(367,11);(363,11);(360,11);(358,11);(356,11);(0,8);];
        [(43,8);(20,7);(19,7);(17,7);(15,7);(13,7);(11,7);(9,7);(7,7);(6,7);(4,7);(7,8);(5,8);(3,8);(1,8);(3,4);];
    ]

    let bigValueLinbit = [|
        0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
        1;2;3;4;6;8;10;13;4;5;6;7;8;9;11;13
    |]

    let bigValueMax = [|
        1;  2;  3;  3;  0;  4;  4;  6;  6;  6;  8;  8;  8;  16; 0;  16;
        16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16; 16
    |]

    let getHuffmanTable id = 
        match id < 32 with
        |true -> 
            match id with
            |0|4|14 -> table0
            |1 -> table1
            |2 -> table2
            |3 -> table3
            |5 -> table5
            |6 -> table6
            |7 -> table7
            |8 -> table8
            |9 -> table9
            |10 -> table10
            |11 -> table11
            |12 -> table12
            |13 -> table13
            |15 -> table15
            |16|17|18|19|20|21|22|23 -> table16
            |_ -> table24
        |false -> failwith "Table ID should be between 0-31"