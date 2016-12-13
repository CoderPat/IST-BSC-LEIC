DROP TABLE IF EXISTS wh_dimensao_user;
DROP TABLE IF EXISTS wh_dimensao_data;
DROP TABLE IF EXISTS wh_dimensao_tempo;
DROP TABLE IF EXISTS wh_dimensao_localizacao;
DROP TABLE IF EXISTS wh_informacao;

--necessary user_id in this case?
CREATE TABLE wh_dim_user(
	user_id int NOT NULL AUTO_INCREMENT,
    nif varchar(9) NOT NULL unique,
    nome varchar(80) NOT NULL,
    telefone varchar(26) NOT NULL,
	PRIMARY KEY (user_id)
)

CREATE TABLE wh_dim_data(
	data_id int NOT NULL AUTO_INCREMENT,
    dia tinyint unsigned NOT NULL,
    semana tinyint unsigned NOT NULL
    mes tinyint unsigned NOT NULL,
    semestre tinyint unsigned NOT NULL,
    ano int unsigned NOT NULL,
	PRIMARY KEY (data_id)
)

CREATE TABLE wh_dimensao_tempo(
    tempo_id int unsigned NOT NULL AUTO_INCREMENT,
    hour tinyint unsigned NOT NULL,
    day tinyint unsigned NOT NULL,
	PRIMARY KEY (time_id)
)

CREATE TABLE wh_dim_localizacao(
	local_id int unsigned NOT NULL AUTO_INCREMENT,
	morada varchar(255) NOT NULL,
	codigo_espaco varchar(255) NOT NULL,
	codigo_posto varchar(255),
	PRIMARY KEY (local_id)
)

--only paid reservations
DELIMITER $$

CREATE PROCEDURE carregar_dim_datas(ano) 
BEGIN
START TRANSACTION
	DROP TABLE IF EXISTS wh_dim_data;

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
	DROP TABLE IF EXISTS wh_dim_tempo;

	DECLARE curr_ts timestamp;
	SET curr_ts = timestamp('0000-01-01 00:00:00'); 
	WHILE (DAY(curr_ts)!=2) DO
		INSERT INTO wh_dim_tempo (hora, segundo) VALUES (HOUR(curr_ts), SECOND(curr_ts)); -- IS THE HIERARCHY STRICT? (second only between 1-60?)
		SET curr_ts = DATE_ADD(curr_ts, INTERVAL 1 SECOND);
	END WHILE;
COMMIT;
END$$


CREATE PROCEDURE carregar_dim_user()
BEGIN
START TRANSACTION
	DROP TABLE IF EXISTS wh_dim_user;

	INSERT INTO wh_dim_user (nif, nome, telefone) 
		SELECT *
		FROM User;
COMMIT;
END$$

CREATE PROCEDURE carregar_dim_user()
BEGIN
START TRANSACTION
	DROP TABLE IF EXISTS wh_dim_user;

	INSERT INTO wh_dim_user (nif, nome, telefone) 
		SELECT nif, nome, telefone
		FROM User;
COMMIT;
END$$

CREATE PROCEDURE carregar_dim_localizacao()
BEGIN
START TRANSACTION
	DROP TABLE IF EXISTS wh_dim_localizacao;

	INSERT INTO wh_dim_localizacao (morada, codigo_espaco, codigo_posto) 
		SELECT morada, codigo, null
		FROM Espaco
		UNION
		SELECT morada, codigo_espaco, codigo
		FROM Posto
COMMIT;
END$$

DELIMITER ;






