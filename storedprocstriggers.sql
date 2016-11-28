DROP TRIGGER IF EXISTS inserir_oferta;
DROP TRIGGER IF EXISTS pagar_reserva;

DELIMITER $$

/*CREATE TRIGGER inserir_oferta BEFORE INSERT 
ON Oferta
FOR EACH ROW
BEGIN
	IF EXISTS ( SELECT 1 FROM Oferta WHERE (NEW.data_fim >= data_inicio) AND (data_fim >= NEW.data_inicio) )
	THEN
		SIGNAL SQLSTATE	'45000';
	END IF;
END$$ */

CREATE TRIGGER pagar_reserva BEFORE INSERT
ON Paga
FOR EACH ROW
BEGIN
	IF ( SELECT MAX(date(timestamp)) FROM Estado WHERE numero=NEW.numero ) > NEW.data
	THEN
		SIGNAL SQLSTATE	'45000';
	END IF;
END$$

DELIMITER ;