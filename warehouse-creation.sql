DROP TABLE IF EXISTS wh_dimensao_user;
DROP TABLE IF EXISTS wh_dimensao_data;
DROP TABLE IF EXISTS wh_dimensao_tempo;
DROP TABLE IF EXISTS wh_dimensao_localizacao;
DROP TABLE IF EXISTS wh_informacao;

--necessary user_id in this case?
CREATE TABLE wh_dimensao_user(
	user_id int NOT NULL AUTO_INCREMENT,
    nif varchar(9) NOT NULL unique,
    nome varchar(80) NOT NULL,
    telefone varchar(26) NOT NULL,
	PRIMARY KEY (user_id)
)

--do all possible days need to be created bedore hand
CREATE TABLE wh_dimensao_data(
	data_id int NOT NULL AUTO_INCREMENT,
    dia tinyint unsigned NOT NULL,
    semana tinyint unsigned NOT NULL
    mes tinyint unsigned NOT NULL,
    semestre tinyint unsigned NOT NULL,
    ano int unsigned NOT NULL,
	PRIMARY KEY (data_id)
)

--only paid reservations
CREATE PROCEDURE criar_datas(ano) 
BEGIN
START TRANSACTION
	DECLARE dt date;
	SET dt = DATE(ano); -- pode estar errado
	WHILE (YEAR(dt)!=ano+1) DO
		INSERT INTO wh_dimensao_data (dia, semana, mes, semestre, ano) VALUES (DAY(dt), WEEK(dt), MONTH(dt), SEMESTER(dt), ano);
		SET dt = DATEADD(dt, INTERVAL 1 DAY);
	END WHILE;
COMMIT;
END;



