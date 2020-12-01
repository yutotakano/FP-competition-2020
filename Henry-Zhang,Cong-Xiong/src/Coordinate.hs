module Coordinate where

import Graphics.Gloss



darkBrown = makeColorI 193 165 141 255 -- darkBrown
pureWhite = makeColorI 255 255 255 255 -- pureWhite
pureGreen = makeColorI 112 165 123 255 -- pureGreen
--brownC = makeColorI 192 148 103 255 -- 
softBrown = makeColorI 249 246 213 255 -- softBrown
neutralBrown = makeColorI 255 232 177 240 -- neutralBrown



coorLeft :: [(Float, Float, Color , Float)]
coorLeft = [
    --t1
    (361,66,darkBrown,3),
    (357,72,darkBrown,3),
    (353,80,darkBrown,3),
    (350,89,darkBrown,3),
    (345,98,darkBrown,3),
    (342,107,darkBrown,3),
    (339,115,darkBrown,3),
    (337,124,darkBrown,3),
    
    --t2
    (267,98,pureWhite,3),
    (276,103,pureWhite,3),
    (283,107,pureWhite,3),

    (291,112,pureWhite,3),
    (298,116,pureWhite,3),
    (305,121,pureWhite,3),
    
    (311,124,pureWhite,3),
    (316,128,pureWhite,3),
    (322,132,pureWhite,3),
    
    (327,136,pureWhite,3),
    (332,140,pureWhite,3),
    (336,144,pureWhite,3),

    (341,148,pureWhite,3),
    (346,152,pureWhite,3),
    (350,156,pureWhite,3),

    (355,160,pureWhite,3),
    (360,165,pureWhite,3),
    (365,170,pureWhite,3),

    (370,175,pureWhite,3),
    (375,180,pureWhite,3),

    --t3

    (260,151,pureWhite,3),
    (270,150,pureWhite,3),
    (277,149,pureWhite,3),

    (285,149,pureWhite,3),
    (292,149,pureWhite,3),
    (299,149,pureWhite,3),

    (305,150,pureWhite,3),
    (311,151,pureWhite,3),
    (317,153,pureWhite,3),

    (323,155,pureWhite,3),
    (329,158,pureWhite,3),
    (334,161,pureWhite,3),

    (340,164,pureWhite,3),
    (345,167,pureWhite,3),
    (351,170,pureWhite,3),

    (356,173,pureWhite,3),
    (360,177,pureWhite,3),
    (366,181,pureWhite,3),

    (370,185,pureWhite,3),

    --t4
    (268,177,pureWhite,3),
    (277,176,pureWhite,3),
    (286,176,pureWhite,3),

    (294,176,pureWhite,3),
    (302,176,pureWhite,3),
    (310,176,pureWhite,3),

    (316,177,pureWhite,3),
    (322,179,pureWhite,3),
    (329,180,pureWhite,3),

    (335,182,pureWhite,3),
    (341,185,pureWhite,3),
    (347,187,pureWhite,3),

    (353,190,pureWhite,3),
    (358,192,pureWhite,3),
    (364,195,pureWhite,3),

    (370,197,pureWhite,3),

    --t5
    (391,155,softBrown,4),
    (398,162,softBrown,5),
    (389,162,softBrown,4),

    (391,169,softBrown,4),
    (400,172,softBrown,4),
    (386,176,softBrown,3.5),

    (394,178,softBrown,4),
    (382,183,softBrown,3.5),
    (389,185,softBrown,3.5),

    (379,188,softBrown,3),
    (385,191,softBrown,3),
    (389,185,softBrown,3.5),

    (379,197,softBrown,5),
    (375,206,softBrown,4),
    (372,214,softBrown,3.5),

    (372,221,softBrown,3),
    (373,227,softBrown,3),

    --t6
    (406,190,softBrown,3),
    (402,196,softBrown,3),
    (399,203,softBrown,3),

    --t7
    (339,200,pureWhite,3),
    (336,206,pureWhite,3),
    (343,206,pureWhite,3),

    (337,213,pureWhite,3.5),
    (345,212,pureWhite,3.5),
    (341,220,pureWhite,4),

    (349,218,pureWhite,3.5),
    (346,225,pureWhite,3),
    (353,224,pureWhite,3.5),

    (360,224,pureWhite,3),
    (366,225,pureWhite,3),

    --t8
    (377,232,darkBrown,3),
    (380,237,darkBrown,3),
    (383,243,darkBrown,3),

    (387,248,darkBrown,3.2),
    (393,254,darkBrown,3.3),
    (397,260,darkBrown,3),

    --t9
    (359,238,pureWhite,3),
    (356,247,pureWhite,7),
    (360,258,pureWhite,5),

    (366,266,pureWhite,5),
    (373,274,pureWhite,4.5),
    (380,279,pureWhite,4.5),

    (389,283,pureWhite,5),
    (390,292,pureWhite,3.5),
    (390,299,pureWhite,3),

    (388,305,pureWhite,3),

    --t10
    (373,239,pureGreen,3),
    (375,245,pureGreen,3),
    (379,251,pureGreen,3),

    (382,256,pureGreen,3),
    (387,259,pureGreen,3),

    --y10
    (398,266,softBrown,3),
    (398,273,softBrown,3),
    (399,279,softBrown,3),
    
    (399,286,softBrown,3),

    --y5
    
    (327,303,softBrown,3),
    (328,310,softBrown,3.3),
    (331,318,softBrown,4),

    (334,327,softBrown,5),
    (337,337,softBrown,5),
    (341,346,softBrown,5),

    (345,357,softBrown,6.5),
    (349,370,softBrown,6.5),
    (354,381,softBrown,6.5),

    (358,394,softBrown,6.5),

    --y6

    (394,329,neutralBrown,3),
    (392,336,neutralBrown,3),
    (390,343,neutralBrown,3),

    (388,349,neutralBrown,3.5),
    (385,357,neutralBrown,3.5),
    (383,365,neutralBrown,4),

    (380,374,neutralBrown,4),
    (377,382,neutralBrown,4),
    (374,392,neutralBrown,4.5),

    (371,402,neutralBrown,4.5),
    (369,412,neutralBrown,4.5),
    (366,422,neutralBrown,4.5),

    (364,433,neutralBrown,4.5),
    (363,443,neutralBrown,4.5),
    (361,453,neutralBrown,4),

    (360,462,neutralBrown,4),
    (359,470,neutralBrown,4),
    (359,480,neutralBrown,4),

    (359,488,neutralBrown,3),
    (359,496,neutralBrown,3),
    (360,504,neutralBrown,3),

    (361,511,neutralBrown,3),

    --y7
    (230,403,pureWhite,3),
    (239,404,pureWhite,3),
    (247,405,pureWhite,3),

    (254,408,pureWhite,3),
    (262,412,pureWhite,3),
    (268,416,pureWhite,3),

    (274,419,pureWhite,3),
    (280,423,pureWhite,3),
    (285,427,pureWhite,3),

    (290,431,pureWhite,3),
    (295,435,pureWhite,3),
    (301,439,pureWhite,3),

    (306,443,pureWhite,3),
    (311,447,pureWhite,3),
    (317,451,pureWhite,3),

    (322,453,pureWhite,3),
    (328,457,pureWhite,3),
    (335,459,pureWhite,3),
    
    (341,461,pureWhite,3),
    (347,464,pureWhite,3),
    (353,466,pureWhite,3),

    --y8
    (491,350,darkBrown,3),
    (490,357,darkBrown,3.5),
    (489,366,darkBrown,3.5),

    (489,376,darkBrown,4),
    (488,387,darkBrown,4),
    (487,397,darkBrown,3.5),

    (485,406,darkBrown,3),
    (483,415,darkBrown,3),
    (481,425,darkBrown,3),

    --s2
    (363,532,pureWhite,3),
    (366,544,pureWhite,3),
    (371,552,pureWhite,3),

    (375,559,pureWhite,3),
    (379,565,pureWhite,3.5),
    (384,571,pureWhite,3.5),

    (390,577,pureWhite,4.5),
    (397,583,pureWhite,4.5),
    (405,589,pureWhite,5),

    (414,595,pureWhite,5),
    (424,599,pureWhite,5),
    (433,602,pureWhite,5),

    (442,603,pureWhite,4.5),
    (451,600,pureWhite,4.5),
    (458,595,pureWhite,4.5),

    (460,586,pureWhite,4.5),
    (457,578,pureWhite,4.5),
    (452,571,pureWhite,3.5),

    (446,565,pureWhite,3.5),
    (440,562,pureWhite,3),
    (433,560,pureWhite,3.5),

    (426,556,pureWhite,4),
    (418,550,pureWhite,4),
    (411,542,pureWhite,4),

    (405,534,pureWhite,3.5),
    (399,528,pureWhite,3.5),
    (392,522,pureWhite,3.5),

    (384,517,pureWhite,3),
    (379,512,pureWhite,3),
    (374,508,pureWhite,3),

    --s1
    (518,477,pureWhite,3),
    (521,483,pureWhite,3),
    (523,490,pureWhite,3.5),

    (524,499,pureWhite,4),
    (520,508,pureWhite,4.5),
    (512,515,pureWhite,4.5),

    (503,523,pureWhite,6),
    (493,531,pureWhite,6),
    (483,540,pureWhite,6),

    (475,549,pureWhite,4.5),
    (468,557,pureWhite,4.5),
    (462,565,pureWhite,4.5),

    (467,575,pureWhite,4.5),
    (473,582,pureWhite,4),
    (481,587,pureWhite,4),

    (489,592,pureWhite,3.5),
    (497,595,pureWhite,3.5),
    (497,595,pureWhite,3.5),

    (506,597,pureWhite,3.5),
    (513,598,pureWhite,3),
    (521,600,pureWhite,3),

    --s3
    (444,623,pureWhite,6.5),
    (450,634,pureWhite,6.5),
    (458,645,pureWhite,6.5),

    (467,653,pureWhite,6.5),
    (477,661,pureWhite,5),
    (486,666,pureWhite,5),

    (495,670,pureWhite,5),
    (506,673,pureWhite,5),
    (515,675,pureWhite,4.5),

    (525,676,pureWhite,4.5),
    (534,676,pureWhite,4.5),
    (543,676,pureWhite,4.5),

    (553,675,pureWhite,4.5),
    (561,672,pureWhite,4.5),
    (570,669,pureWhite,4.5),

    (579,665,pureWhite,5),
    (589,660,pureWhite,5),
    (598,654,pureWhite,5),

    (606,648,pureWhite,5),
    (614,641,pureWhite,5),
    (622,635,pureWhite,5),

    --y9
    (383, 503, neutralBrown, 6.5),
    (393, 504, neutralBrown, 3.5),
    (388, 511, neutralBrown, 3),

    (400, 508, neutralBrown, 4),
    (394, 514, neutralBrown, 3.5),
    (409, 511, neutralBrown, 4),

    (402, 518, neutralBrown, 4.5),
    (418, 510, neutralBrown, 4.5),
    (413, 520, neutralBrown, 4.5),

    (428, 508, neutralBrown, 4),
    (424, 518, neutralBrown, 4.5),
    (434, 515, neutralBrown, 4.5),
    (437, 506, neutralBrown, 4.5),

    (446, 501, neutralBrown, 4.5),
    (446, 513, neutralBrown, 6.5),
    (456, 499, neutralBrown, 4),

    (456, 508, neutralBrown, 4),
    (464, 500, neutralBrown, 3.5),
    (464, 508, neutralBrown, 3),

    (471, 505, neutralBrown, 4.5),
    (479, 507, neutralBrown, 3),
    (407, 525, neutralBrown, 3),

    (414, 529, neutralBrown, 4),
    (423, 528, neutralBrown, 4),
    (432, 525, neutralBrown, 4),

    (419, 536, neutralBrown, 4),
    (427, 534, neutralBrown, 3.5),
    (434, 532, neutralBrown, 3.5),

    (441, 523, neutralBrown, 4),
    (442, 533, neutralBrown, 4),
    (425, 543, neutralBrown, 4),

    (437, 539, neutralBrown, 3.5),
    (445, 540, neutralBrown, 3.5),
    (431, 549, neutralBrown, 3.5),

    (441, 545, neutralBrown, 3),
    (446, 540, neutralBrown, 3.5),
    (436, 554, neutralBrown, 3),

    (448, 549, neutralBrown, 4.5),
    (455, 556, neutralBrown, 4),

    --t1
    (345, 404, pureWhite, 3),
    (345, 411, pureWhite, 3),
    (346, 418, pureWhite, 3),

    (349, 424, pureWhite, 3),
    (352, 429, pureWhite, 3),
    (357, 434, pureWhite, 3),

    --t2
    (317, 437, pureWhite, 3),
    (325, 437, pureWhite, 3),
    (331, 438, pureWhite, 3),

    (338, 440, pureWhite, 3),
    (344, 441, pureWhite, 3),
    (350, 443, pureWhite, 3),

    (356, 445, pureWhite, 3),

    --t3
    (352, 474, pureWhite, 3),
    (346, 472, pureWhite, 3),
    (341, 481, pureWhite, 3),

    (335, 480, pureWhite, 3),
    (328, 482, pureWhite, 3),
    (323, 486, pureWhite, 3),

    (318, 491, pureWhite, 3),

    --t4
    (353, 483, pureWhite, 3),
    (346, 484, pureWhite, 3),
    (341, 487, pureWhite, 3),

    (335, 491, pureWhite, 3),
    (331, 495, pureWhite, 3),
    (327, 501, pureWhite, 3),

    (324, 506, pureWhite, 3),
    (320, 512, pureWhite, 3),
    (315, 517, pureWhite, 3),

    (309, 522, pureWhite, 3),

    --t5
    (354, 508, pureWhite, 3),
    (348, 510, pureWhite, 3),
    (342, 512, pureWhite, 3),

    (336, 515, pureWhite, 3),
    (332, 520, pureWhite, 3),
    (326, 533, pureWhite, 3),

    (323, 540, pureWhite, 3),
    (329, 526, pureWhite, 3),

    --t6
    (360, 546, pureWhite, 3),
    (354, 548, pureWhite, 3),
    (348, 551, pureWhite, 3),

    (342, 554, pureWhite, 3),
    (337, 559, pureWhite, 3),
    (332, 564, pureWhite, 3),

    --t7
    (352, 562, pureWhite, 3),
    (348, 568, pureWhite, 3),
    (343, 575, pureWhite, 3),

    (340, 583, pureWhite, 3),
    (337, 592, pureWhite, 3),

    --t8
    (389, 560, pureWhite, 3.5),
    (376, 575, pureWhite, 3),
    (370, 579, pureWhite, 3),

    (364, 582, pureWhite, 3),
    (358, 587, pureWhite, 3),
    (352, 592, pureWhite, 3),

    --t9
    (408, 577, pureWhite, 3),
    (389, 595, pureWhite, 3),
    (385, 601, pureWhite, 3),

    (381, 611, pureWhite, 3),

    --t10
    (530, 581, pureWhite, 3),
    (532, 587, pureWhite, 3),
    (536, 592, pureWhite, 3),
    
    (538, 599, pureWhite, 3),
    (542, 605, pureWhite, 3),
    (544, 612, pureWhite, 3),

    --x1
    (553, 581, pureWhite, 3),
    (558, 586, pureWhite, 3),
    (562, 591, pureWhite, 3),

    (565, 596, pureWhite, 3),
    (570, 603, pureWhite, 3),
    (573, 608, pureWhite, 3),

    (576, 617, pureWhite, 3),

    --x2
    (566, 572, pureWhite, 3),
    (572, 575, pureWhite, 3),
    (577, 579, pureWhite, 3),

    (582, 583, pureWhite, 3),
    (588, 586, pureWhite, 3),
    (593, 589, pureWhite, 3),
    

    (598, 593, pureWhite, 3),
    (603, 597, pureWhite, 3),
    (608, 600, pureWhite, 3),

    (614, 604, pureWhite, 3),
    (619, 608, pureWhite, 3),
    (625, 612, pureWhite, 3),

    (631, 617, pureWhite, 3),
    (637, 622, pureWhite, 3),

    --x3
    (592, 555, pureWhite, 3),
    (597, 558, pureWhite, 3),
    (602, 561, pureWhite, 3),

    (608, 564, pureWhite, 3),
    (612, 568, pureWhite, 3),
    (621, 582, pureWhite, 3),

    (625, 589, pureWhite, 3),
    (628, 595, pureWhite, 3),
    (632, 602, pureWhite, 3),

    (636, 611, pureWhite, 3),

    --x4
    (634, 569, pureWhite, 3),
    (641, 570, pureWhite, 3),
    (648, 571, pureWhite, 3),

    (654, 572, pureWhite, 3),

    (660, 572, pureWhite, 3),
    (666, 573, pureWhite, 3),
    (672, 573, pureWhite, 3),

    (679, 573, pureWhite, 3),
    (684, 573, pureWhite, 3),
    (691, 573, pureWhite, 3),

    (697, 574, pureWhite, 3),
    (703, 574, pureWhite, 3),
    (710, 574, pureWhite, 3),
    
    (716, 575, pureWhite, 3),
    (723, 574, pureWhite, 3),
    (730, 575, pureWhite, 3),

    (737, 575, pureWhite, 3),

    --x5

    (645, 557, pureWhite, 3),
    (652, 557, pureWhite, 3),
    (658, 557, pureWhite, 3),
    
    (665, 557, pureWhite, 3),
    (671, 557, pureWhite, 3),
    (677, 557, pureWhite, 3),

    (684, 557, pureWhite, 3),
    (690, 557, pureWhite, 3),
    (696, 557, pureWhite, 3),

    (703, 556, pureWhite, 3),
    (710, 556, pureWhite, 3),
    (716, 557, pureWhite, 3),

    (723, 556, pureWhite, 3),
    (730, 556, pureWhite, 3),
    (737, 555, pureWhite, 3),

    (745, 554, pureWhite, 3),
    (752, 553, pureWhite, 3),
    (761, 552, pureWhite, 3),

    --x6
    (590, 537, pureWhite, 3),
    (596, 537, pureWhite, 3),
    (602, 537, pureWhite, 3),

    (608, 538, pureWhite, 3),
    (614, 539, pureWhite, 3),
    (620, 539, pureWhite, 3),

    (627, 540, pureWhite, 3),
    (632, 541, pureWhite, 3),
    (638, 541, pureWhite, 3),

    (644, 542, pureWhite, 3),
    (651, 544, pureWhite, 3),
    (657, 544, pureWhite, 3),

    (664, 545, pureWhite, 3),
    (670, 546, pureWhite, 3),
    (678, 548, pureWhite, 3),

    (686, 548, pureWhite, 3),

    --x7
    (585, 558, pureWhite, 3),
    (589, 563, pureWhite, 3),
    (595, 567, pureWhite, 3),
    
    (601, 570, pureWhite, 3),
    (607, 574, pureWhite, 3),
    (614, 575, pureWhite, 3),

    (620, 576, pureWhite, 3),
    (625, 578, pureWhite, 3),
    (631, 581, pureWhite, 3),

    (637, 583, pureWhite, 3),
    (643, 585, pureWhite, 3),
    (648, 587, pureWhite, 3),

    (654, 590, pureWhite, 3),
    (660, 592, pureWhite, 3),
    (666, 594, pureWhite, 3),

    (672, 597, pureWhite, 3),
    (678, 599, pureWhite, 3),
    (686, 600, pureWhite, 3),

    (693, 602, pureWhite, 3),
    (701, 604, pureWhite, 3),
    (709, 604, pureWhite, 3),

    --x8
    (633, 530, pureWhite, 3),
    (640, 530, pureWhite, 3),
    (648, 530, pureWhite, 3),
    (655, 530, pureWhite, 3),
    
    (661, 531, pureWhite, 3),
    (667, 531, pureWhite, 3),
    (674, 531, pureWhite, 3),

    (680, 531, pureWhite, 3),
    (687, 532, pureWhite, 3),
    (693, 532, pureWhite, 3),

    (700, 532, pureWhite, 3),
    (706, 533, pureWhite, 3),
    (712, 534, pureWhite, 3),
    (719, 534, pureWhite, 3),

    (725, 536, pureWhite, 3),
    (731, 537, pureWhite, 3),
    (738, 538, pureWhite, 3),

    (744, 540, pureWhite, 3),

    (750, 542, pureWhite, 3),
    (756, 543, pureWhite, 3),
    (762, 545, pureWhite, 3),

    (768, 547, pureWhite, 3),
    (774, 548, pureWhite, 3),
    (779, 550, pureWhite, 3),

    (786, 552, pureWhite, 3),
    (792, 554, pureWhite, 3),
    (799, 556, pureWhite, 3),

    (806, 558, pureWhite, 3),
    (813, 560, pureWhite, 3),
    (821, 561, pureWhite, 3),

    (828, 562, pureWhite, 3)

    ]