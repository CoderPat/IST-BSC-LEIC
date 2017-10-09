/*TODO: Replace 2016_Timestamp*/

--1
SELECT * 
FROM espaco
WHERE (morada, codigo) NOT IN 
(
	SELECT DISTINCT morada, codigo_espaco AS codigo
	FROM posto p NATURAL JOIN aluga a NATURAL JOIN estado e
	WHERE estado="Aceite" 
);

--2
SELECT morada
FROM alugavel NATURAL JOIN aluga
GROUP BY morada
HAVING COUNT(numero) > 
(
	SELECT AVG(n_reservas)
	FROM 
	(
		SELECT COUNT(numero) as n_reservas
		FROM alugavel NATURAL JOIN aluga
		GROUP BY morada
	) as num_r
);


--3
SELECT nif
FROM 
(
	SELECT DISTINCT nif, id
	FROM arrenda NATURAL JOIN fiscaliza
) as ed_fis
GROUP BY nif
HAVING COUNT(id) <= 1;

--4
SELECT morada, codigo, SUM(custo)
FROM 
(
	SELECT morada, codigo, tarifa*DATEDIFF(data_fim,data_inicio) as custo
	FROM espaco NATURAL JOIN oferta NATURAL JOIN aluga NATURAL JOIN paga
	WHERE YEAR(data) = '2016'
	UNION
	SELECT morada, codigo_espaco AS codigo, tarifa*DATEDIFF(data_fim,data_inicio) as custo
	FROM posto NATURAL JOIN oferta NATURAL JOIN aluga NATURAL JOIN paga
	WHERE YEAR(data) = '2016'
) as all_custos
GROUP BY morada, codigo;

--5 
SELECT * 
FROM espaco 
WHERE (morada, codigo) NOT IN
(
	SELECT morada, codigo_espaco AS codigo 
	FROM posto
	WHERE (morada, codigo, codigo_espaco) NOT IN
	(
		SELECT DISTINCT morada, codigo, codigo_espaco
		FROM posto NATURAL JOIN aluga NATURAL JOIN estado
		WHERE estado="aceite"
	)
);
