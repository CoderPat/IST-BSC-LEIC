# BD
A repository for the BD college class

Need to save this somewhere.

CREATE TABLE Alugavel (
	morada	CHAR(50),
	codigo	INT,
	foto	INT
)

CREATE TABLE Oferta (
	morada	CHAR(50),
	codigo	INT,
	data_inicio	DATETIME,
	data_fim	DATETIME,
	tarifa	FLOAT
)

INSERT INTO Alugavel (morada, codigo, foto) VALUES ('morada1', 1337, 1)
INSERT INTO Alugavel (morada, codigo, foto) VALUES ('morada2', 1455, 2)
INSERT INTO Alugavel (morada, codigo, foto) VALUES ('morada3', 9483, 3)
INSERT INTO Alugavel (morada, codigo, foto) VALUES ('morada4', 3241, 4)

INSERT INTO Oferta (morada, codigo, data_inicio, data_fim, tarifa) VALUES ('morada1', 1337, '9-11-2001 10:00:00', '10-11-2001 10:00:00', 1.01)
INSERT INTO Oferta (morada, codigo, data_inicio, data_fim, tarifa) VALUES ('morada1', 1337, '9-11-2002 10:00:00', '10-11-2003 10:00:00', 41)
INSERT INTO Oferta (morada, codigo, data_inicio, data_fim, tarifa) VALUES ('morada3', 9483, '9-11-2015 10:00:00', '9-11-2016 10:00:00', 13.37)
INSERT INTO Oferta (morada, codigo, data_inicio, data_fim, tarifa) VALUES ('morada2', 1455, '9-11-2021 10:00:00', '10-11-2031 10:00:00', 69.69)



