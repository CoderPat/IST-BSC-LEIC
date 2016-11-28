DROP TABLE IF EXISTS Estado;
DROP TABLE IF EXISTS Paga;
DROP TABLE IF EXISTS Aluga;
DROP TABLE IF EXISTS Reserva;
DROP TABLE IF EXISTS Oferta;
DROP TABLE IF EXISTS Posto;
DROP TABLE IF EXISTS Espaco;
DROP TABLE IF EXISTS Fiscaliza;
DROP TABLE IF EXISTS Arrenda;
DROP TABLE IF EXISTS Alugavel;
DROP TABLE IF EXISTS Edificio;
DROP TABLE IF EXISTS Fiscal;
DROP TABLE IF EXISTS User;

CREATE TABLE User (
    `nif` int(9) unsigned NOT NULL,
    `nome` varchar(100) NOT NULL,
    `telefone` varchar(15) NOT NULL,
    PRIMARY KEY (`nif`)
);

CREATE TABLE Fiscal (
    `id` int unsigned NOT NULL AUTO_INCREMENT,
    `empresa` varchar(100) NOT NULL,
    PRIMARY KEY (`id`)
);

CREATE TABLE Edificio (
    `morada` varchar(100) NOT NULL,
    PRIMARY KEY (`morada`)
);

CREATE TABLE Alugavel(
    `morada` varchar(100) NOT NULL, 
    `codigo` varchar(32) NOT NULL, 
    `foto` varchar(255) NOT NULL,
    PRIMARY KEY (`morada`, `codigo`),
    FOREIGN KEY (`morada`) REFERENCES Edificio(`morada`)
);

CREATE TABLE Arrenda(
    `morada` varchar(100) NOT NULL, 
    `codigo` varchar(32) NOT NULL,
    `nif` int(9) unsigned NOT NULL,
    PRIMARY KEY (`morada`, `codigo`),
    FOREIGN KEY (`morada`, `codigo`) REFERENCES Alugavel(`morada`, `codigo`),
    FOREIGN KEY (`nif`) REFERENCES User(`nif`)
);

CREATE TABLE Fiscaliza(
    `id` int unsigned NOT NULL, 
    `morada` varchar(100) NOT NULL, 
    `codigo` varchar(32) NOT NULL,
    PRIMARY KEY (`id`, `morada`, `codigo`),
    FOREIGN KEY (`id`) REFERENCES Fiscal(`id`),
    FOREIGN KEY (`morada`, `codigo`) REFERENCES Arrenda(`morada`, `codigo`)
);

CREATE TABLE Espaco(
    `morada` varchar(100) NOT NULL, 
    `codigo` varchar(32) NOT NULL,
    PRIMARY KEY (`morada`, `codigo`),
    FOREIGN KEY (`morada`, `codigo`) REFERENCES Alugavel(`morada`, `codigo`)
);

CREATE TABLE Posto(
    `morada` varchar(100) NOT NULL, 
    `codigo` varchar(32) NOT NULL, 
    `codigo_espaco` varchar(32) NOT NULL,
    PRIMARY KEY(`morada`, `codigo`),
    FOREIGN KEY (`morada`, `codigo`) REFERENCES Alugavel(`morada`, `codigo`),
    FOREIGN KEY (`morada`, `codigo_espaco`) REFERENCES Espaco(`morada`, `codigo`)
);

CREATE TABLE Oferta(
    `morada` varchar(100) NOT NULL, 
    `codigo` varchar(32) NOT NULL, 
    `data_inicio` DATE NOT NULL,
    `data_fim` DATE NOT NULL, 
    `tarifa` int(3) NOT NULL,
    PRIMARY KEY(`morada`, `codigo`, `data_inicio`),
    FOREIGN KEY (`morada`, `codigo`) REFERENCES Alugavel(`morada`, `codigo`)
);

CREATE TABLE Reserva (
    `numero` int unsigned NOT NULL AUTO_INCREMENT,
    PRIMARY KEY(`numero`)
);

CREATE TABLE Aluga(
    `morada` varchar(100) NOT NULL, 
    `codigo` varchar(32) NOT NULL, 
    `data_inicio` DATE NOT NULL,
    `nif` int(9) unsigned NOT NULL,
    `numero` int unsigned NOT NULL,
    PRIMARY KEY (`morada`, `codigo`,`data_inicio`,`nif`,`numero`),
    FOREIGN KEY (`morada`, `codigo`, `data_inicio`) REFERENCES Oferta(`morada`, `codigo`, `data_inicio`),
    FOREIGN KEY (`nif`) REFERENCES User(`nif`),
    FOREIGN KEY (`numero`) REFERENCES Reserva(`numero`)
);

CREATE TABLE Paga(
    `numero` int unsigned NOT NULL,
    `data` DATE NOT NULL, 
    `metodo` enum ('dollabill', 'cleditecalde'),
    PRIMARY KEY (`numero`),
    FOREIGN KEY (`numero`) REFERENCES Reserva(`numero`)
);

CREATE TABLE Estado(
    `numero` int unsigned NOT NULL, 
    `timestamp` timestamp NOT NULL, 
    `estado` enum ('aceite', 'rejeitado'), 
    PRIMARY KEY (`numero`, `timestamp`),
    FOREIGN KEY (`numero`) REFERENCES Reserva(`numero`)
);


