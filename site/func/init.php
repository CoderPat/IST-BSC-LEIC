<?php
//Este ficheiro vai ser incluido por todas as paginas no inicio
ini_set('display_startup_errors', 1);
ini_set('display_errors', 1);
error_reporting(E_ALL);
$root = "/afs/.ist.utl.pt/users/6/1/ist181861/web/bd/site/";
$webroot = "/~ist181861/bd/site";
//TODO: Inicializar ligacao a db

try {
    $host = "db.ist.utl.pt";
    $user ="istxxxxx";
    $password = "xxxxxxx";
    $dbname = $user;
    $db = new PDO("mysql:host=$host;dbname=$dbname", $user, $password);
    $db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
} catch(PDOException $e) {
    echo("<p>ERROR: {$e->getMessage()}</p>");
    exit("Aborting...");
}

?>