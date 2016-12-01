<?php
//Este ficheiro vai ser incluido por todas as paginas no inicio
ini_set('display_startup_errors', 1);
ini_set('display_errors', 1);
error_reporting(E_ALL);
$root = 
$webroot = 

$host = "localhost";
$dbname="DB_database";
$user = 
$password = 

//TODO: Inicializar ligacao a db

try{
	$db = new PDO("mysql:host=$host;dbname=$dbname", $user, $password);
	$db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
} catch(PDOException $e) {
    echo("<p>ERROR: {$e->getMessage()}</p>");
    exit("Aborting..."); 
}
?>