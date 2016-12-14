DROP TABLE IF EXISTS wh_dim_user;
DROP TABLE IF EXISTS wh_dim_data;
DROP TABLE IF EXISTS wh_dim_tempo;
DROP TABLE IF EXISTS wh_dim_localizacao;
DROP TABLE IF EXISTS wh_informacao;

#necessary user_id in this case?
CREATE TABLE wh_dim_user(
	user_id int UNSIGNED NOT NULL AUTO_INCREMENT,
    nif varchar(9) NOT NULL,
    nome varchar(80) NOT NULL,
    telefone varchar(26) NOT NULL,
	PRIMARY KEY (user_id)
);

CREATE TABLE wh_dim_data(
	data_id int unsigned NOT NULL AUTO_INCREMENT,
    dia tinyint unsigned NOT NULL,
    semana tinyint unsigned NOT NULL,
    mes tinyint unsigned NOT NULL,
    semestre tinyint unsigned NOT NULL,
    ano int unsigned NOT NULL,
	PRIMARY KEY (data_id)
);

CREATE TABLE wh_dim_tempo(
    tempo_id int unsigned NOT NULL AUTO_INCREMENT,
    hora tinyint unsigned NOT NULL,
    minuto tinyint unsigned NOT NULL,
	PRIMARY KEY (tempo_id)
);	

CREATE TABLE wh_dim_localizacao(
	local_id int unsigned NOT NULL AUTO_INCREMENT,
	morada varchar(255) NOT NULL,
	codigo_espaco varchar(255) NOT NULL,
	codigo_posto varchar(255),
	PRIMARY KEY (local_id)
);

CREATE TABLE wh_informacao(
	user_id int unsigned NOT NULL,
	data_id int unsigned NOT NULL,
	tempo_id int unsigned NOT NULL,
	local_id int unsigned NOT NULL,
	montante int NOT NULL, 
	FOREIGN KEY (user_id) REFERENCES wh_dim_user(user_id),	
	FOREIGN KEY (data_id) REFERENCES wh_dim_data(data_id),	
	FOREIGN KEY (tempo_id) REFERENCES wh_dim_tempo(tempo_id),
	FOREIGN KEY (local_id) REFERENCES wh_dim_localizacao(local_id)
);

--only paid reservations
DELIMITER $$

CREATE PROCEDURE carregar_dim_datas(ano) 
BEGIN
START TRANSACTION
	DECLARE curr_date date;
	SET curr_date = DATE(ano + '-01-01');
	WHILE (YEAR(curr_date)!=ano+1) DO
		INSERT INTO wh_dim_data (dia, semana, mes, semestre, ano) VALUES (DAY(curr_date), WEEK(curr_date), MONTH(curr_date), SEMESTER(curr_date), ano); -- IS THE HIERARCHY STRICT? (MONTH RANGE ONLY 1-6, WEEK RANGE ONLY 1-5?)
		SET curr_date = DATE_ADD(curr_date, INTERVAL 1 DAY);
	END WHILE;
COMMIT;
END$$


CREATE PROCEDURE carregar_dim_tempo() 
BEGIN
START TRANSACTION
	DECLARE curr_ts timestamp;
	SET curr_ts = timestamp('0000-01-01 00:00:00'); 
	WHILE (DAY(curr_ts)!=2) DO
		INSERT INTO wh_dim_tempo (hora, segundo) VALUES (HOUR(curr_ts), MINUTE(curr_ts)); -- IS THE HIERARCHY STRICT? (second only between 1-60?)
		SET curr_ts = DATE_ADD(curr_ts, INTERVAL 1 SECOND);
	END WHILE;
COMMIT;
END$$


CREATE PROCEDURE carregar_dim_user()
BEGIN
START TRANSACTION
	INSERT INTO wh_dim_user (nif, nome, telefone) 
		SELECT *
		FROM User;
COMMIT;
END$$


CREATE PROCEDURE carregar_dim_localizacao()
BEGIN
START TRANSACTION
	INSERT INTO wh_dim_localizacao (morada, codigo_espaco, codigo_posto) 
		SELECT morada, codigo, null
		FROM Espaco
		UNION
		SELECT morada, codigo_espaco, codigo
		FROM Posto
COMMIT;
END$$

CREATE PROCEDURE carregar_medidas()
BEGIN
START TRANSACTION
	INSERT INTO wh_informacao (user_id, data_id, tempo_id, local_id) 
		SELECT user_id, data_id, tempo_id, local_id, tarifa*(DATADIFF(data_fim,data_inicio)), DATADIFF(data_fim, data_inicio)
		FROM paga NATURAL JOIN oferta INNER JOIN wh_dim_user U
									  INNER JOIN wh_dim_data D
									  INNER JOIN wh_dim_tempo T
									  INNER JOIN wh_dim_localizacao L 
		ON  U.nif = oferta.nif AND
		   (D.dia = DAY(paga.data) AND D.mes = MONTH(paga.data) AND D.ano = YEAR(paga.data)) AND
		   (T.hora = HOUR(paga.data) AND T.minuto = MINUTE(paga.data)) AND
		   (L.morada = oferta.morada AND ((L.codigo_espaco = oferta.codigo AND L.codigo_post IS NULL) OR L.codigo_posto = oferta.codigo))
COMMIT
END$$

DELIMITER;


dia tinyint unsigned NOT NULL,
    semana tinyint unsigned NOT NULL
    mes tinyint unsigned NOT NULL,
    semestre tinyint unsigned NOT NULL,
    ano int unsigned NOT NULL,
    
    morada varchar(255) NOT NULL,
	codigo_espaco varchar(255) NOT NULL,
	codigo_posto varchar(255)

SELECT dia, semana, mes, semestre, ano, morada, codigo_espaco, codigo_posto, AVG(montante) as media
from wh_informacao NATURAL JOIN wh_dim_informacao NATURAL JOIN wh_dim_data  
group by dia, semana, mes, semestre, ano, morada, codigo_espaco, codigo_posto with rollup





