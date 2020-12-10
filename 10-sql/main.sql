DROP SCHEMA IF EXISTS aoc;
CREATE SCHEMA aoc;
USE aoc;
CREATE TABLE Input (Val int(4) NOT NULL);
INSERT INTO Input (Val) VALUES (80);
INSERT INTO Input (Val) VALUES (87);
INSERT INTO Input (Val) VALUES (10);
INSERT INTO Input (Val) VALUES (122);
INSERT INTO Input (Val) VALUES (57);
INSERT INTO Input (Val) VALUES (142);
INSERT INTO Input (Val) VALUES (134);
INSERT INTO Input (Val) VALUES (59);
INSERT INTO Input (Val) VALUES (113);
INSERT INTO Input (Val) VALUES (139);
INSERT INTO Input (Val) VALUES (101);
INSERT INTO Input (Val) VALUES (41);
INSERT INTO Input (Val) VALUES (138);
INSERT INTO Input (Val) VALUES (112);
INSERT INTO Input (Val) VALUES (46);
INSERT INTO Input (Val) VALUES (96);
INSERT INTO Input (Val) VALUES (43);
INSERT INTO Input (Val) VALUES (125);
INSERT INTO Input (Val) VALUES (36);
INSERT INTO Input (Val) VALUES (54);
INSERT INTO Input (Val) VALUES (133);
INSERT INTO Input (Val) VALUES (17);
INSERT INTO Input (Val) VALUES (42);
INSERT INTO Input (Val) VALUES (98);
INSERT INTO Input (Val) VALUES (7);
INSERT INTO Input (Val) VALUES (114);
INSERT INTO Input (Val) VALUES (78);
INSERT INTO Input (Val) VALUES (67);
INSERT INTO Input (Val) VALUES (77);
INSERT INTO Input (Val) VALUES (28);
INSERT INTO Input (Val) VALUES (149);
INSERT INTO Input (Val) VALUES (58);
INSERT INTO Input (Val) VALUES (20);
INSERT INTO Input (Val) VALUES (105);
INSERT INTO Input (Val) VALUES (31);
INSERT INTO Input (Val) VALUES (19);
INSERT INTO Input (Val) VALUES (18);
INSERT INTO Input (Val) VALUES (27);
INSERT INTO Input (Val) VALUES (40);
INSERT INTO Input (Val) VALUES (71);
INSERT INTO Input (Val) VALUES (117);
INSERT INTO Input (Val) VALUES (66);
INSERT INTO Input (Val) VALUES (21);
INSERT INTO Input (Val) VALUES (72);
INSERT INTO Input (Val) VALUES (146);
INSERT INTO Input (Val) VALUES (90);
INSERT INTO Input (Val) VALUES (97);
INSERT INTO Input (Val) VALUES (94);
INSERT INTO Input (Val) VALUES (123);
INSERT INTO Input (Val) VALUES (1);
INSERT INTO Input (Val) VALUES (119);
INSERT INTO Input (Val) VALUES (30);
INSERT INTO Input (Val) VALUES (84);
INSERT INTO Input (Val) VALUES (61);
INSERT INTO Input (Val) VALUES (91);
INSERT INTO Input (Val) VALUES (118);
INSERT INTO Input (Val) VALUES (2);
INSERT INTO Input (Val) VALUES (29);
INSERT INTO Input (Val) VALUES (104);
INSERT INTO Input (Val) VALUES (73);
INSERT INTO Input (Val) VALUES (13);
INSERT INTO Input (Val) VALUES (76);
INSERT INTO Input (Val) VALUES (24);
INSERT INTO Input (Val) VALUES (148);
INSERT INTO Input (Val) VALUES (68);
INSERT INTO Input (Val) VALUES (111);
INSERT INTO Input (Val) VALUES (131);
INSERT INTO Input (Val) VALUES (83);
INSERT INTO Input (Val) VALUES (49);
INSERT INTO Input (Val) VALUES (8);
INSERT INTO Input (Val) VALUES (132);
INSERT INTO Input (Val) VALUES (9);
INSERT INTO Input (Val) VALUES (64);
INSERT INTO Input (Val) VALUES (79);
INSERT INTO Input (Val) VALUES (124);
INSERT INTO Input (Val) VALUES (95);
INSERT INTO Input (Val) VALUES (88);
INSERT INTO Input (Val) VALUES (135);
INSERT INTO Input (Val) VALUES (3);
INSERT INTO Input (Val) VALUES (51);
INSERT INTO Input (Val) VALUES (39);
INSERT INTO Input (Val) VALUES (6);
INSERT INTO Input (Val) VALUES (60);
INSERT INTO Input (Val) VALUES (108);
INSERT INTO Input (Val) VALUES (14);
INSERT INTO Input (Val) VALUES (35);
INSERT INTO Input (Val) VALUES (147);
INSERT INTO Input (Val) VALUES (89);
INSERT INTO Input (Val) VALUES (34);
INSERT INTO Input (Val) VALUES (65);
INSERT INTO Input (Val) VALUES (50);
INSERT INTO Input (Val) VALUES (145);
INSERT INTO Input (Val) VALUES (128);
use aoc;

CREATE OR REPLACE VIEW working AS
  SELECT Val FROM Input
  UNION ALL SELECT 0
  UNION ALL (SELECT MAX(Val) + 3 FROM Input LIMIT 1);

CREATE OR REPLACE VIEW diff1 AS
SELECT Val
FROM working
WHERE Val + 1 IN (SELECT Val FROM working);

CREATE OR REPLACE VIEW diff3 AS
SELECT Val
FROM working
WHERE Val + 3 IN (SELECT Val FROM working)
AND Val NOT IN (SELECT Val FROM diff1);

SELECT plus1.cnt * plus3.cnt as `Part 1`
FROM
(SELECT COUNT(*) `cnt` FROM diff1) as plus1,
(SELECT COUNT(*) `cnt` FROM diff3) as plus3;
