CREATE OR REPLACE VIEW working AS
SELECT Val
FROM
  Input
  UNION ALL SELECT 0
  UNION ALL (SELECT MAX(Val) + 3 FROM Input LIMIT 1);

CREATE OR REPLACE VIEW diff1 AS
SELECT Val
FROM working
WHERE Val + 1 IN (SELECT Val FROM working);

CREATE OR REPLACE VIEW diff3 AS
SELECT Val
FROM working
WHERE
  Val + 3 IN (SELECT Val FROM working)
  AND Val NOT IN (SELECT Val FROM diff1);

SELECT plus1.cnt * plus3.cnt as `Part 1`
FROM
  (SELECT COUNT(*) `cnt` FROM diff1) as plus1,
  (SELECT COUNT(*) `cnt` FROM diff3) as plus3;
