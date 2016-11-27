DROP TABLE User;
DROP TABLE Fiscal;
DROP TABLE Edificio;
DROP TABLE Alugavel;
DROP TABLE Arrenda;
DROP TABLE Fiscaliza;
DROP TABLE Espaco;
DROP TABLE Posto;
DROP TABLE Oferta;
DROP TABLE Aluga;
DROP TABLE Paga;
DROP TABLE Estado;
DROP TABLE Reserva;

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
    FOREIGN KEY (`morada`, `codigo`) Alugavel(`morada`, `codigo`),
    FOREIGN KEY (`nif`) User(`nif`)
);

CREATE TABLE Fiscaliza(
    `id` int unsigned NOT NULL, 
    `morada` varchar(100) NOT NULL, 
    `codigo` varchar(32) NOT NULL,
    PRIMARY KEY (`id`, `morada`, `codigo`),
    FOREIGN KEY (`id`) Fiscal(`id`),
    FOREIGN KEY (`morada`, `codigo`) Arrenda(`morada`, `codigo`)
);

CREATE TABLE Espaco(
    `morada` varchar(100) NOT NULL, 
    `codigo` varchar(32) NOT NULL,
    PRIMARY KEY(`morada`, `codigo`),
    FOREIGN KEY (`morada`, `codigo`) Alugavel(`morada`, `codigo`)
);

CREATE TABLE Posto(
    `Morada` varchar(100) NOT NULL, 
    `codigo` varchar(32) NOT NULL, 
    `codigo_espaco` varchar(32) NOT NULL,
    PRIMARY KEY(`Morada`, `codigo`),
    FOREIGN KEY (`Morada`, `codigo`) Alugavel(`morada`, `codigo`),
    FOREIGN KEY (`Morada`, `codigo_espaco`) Espaco(`morada`, `codigo`)
);

CREATE TABLE Oferta(
    `morada` varchar(100) NOT NULL, 
    `codigo` varchar(32) NOT NULL, 
    `data_inicio` DATE NOT NULL,
    `data_fim` DATE NOT NULL, 
    `tarifa` int(3) NOT NULL,
    PRIMARY KEY(`morada`, `codigo`, `data_inicio`),
    FOREIGN KEY (`Morada`, `codigo`) Alugavel(`morada`, `codigo`)
);

CREATE TABLE Aluga(
    `morada` varchar(100) NOT NULL, 
    `codigo` varchar(32) NOT NULL, 
    `data_inicio` DATE NOT NULL,
    `nif` int(9) unsigned NOT NULL,
    `numero` int unsigned NOT NULL,
    PRIMARY KEY(`morada`, `codigo`,`data_inicio`,`nif`,`numero`),
    FOREIGN KEY(`morada`, `codigo`, `data_inicio`) Oferta(`morada`, `codigo`, `data_inicio`),
    FOREIGN KEY(`nif`) User(`nif`),
    FOREIGN KEY(`numero`) Reserva(`numero`)
);

CREATE TABLE Paga(
    `numero` int unsigned NOT NULL,
    `data` DATE NOT NULL, 
    `metodo` enum ('dollabill', 'cleditecalde'),
    PRIMARY KEY(`numero`),
    FOREIGN KEY(`numero`) Reserva(`numero`)
);

CREATE TABLE Estado(
    `numero` int unsigned NOT NULL, 
    `timestamp` timestamp NOT NULL, 
    `estado` enum NONE,
    PRIMARY KEY(`numero`, `timestamp`),
    FOREIGN KEY(`numero`) Reserva(`numero`)
);

CREATE TABLE Reserva (
    `numero` int unsigned NOT NULL AUTO_INCREMENT,
    PRIMARY KEY(`numero`)
);
