<?php
//Este ficheiro vai ser incluido por todas as paginas no inicio
ini_set('display_startup_errors', 1);
ini_set('display_errors', 1);
error_reporting(E_ALL);
$root = "/afs/.ist.utl.pt/users/6/1/ist181861/web/bd/site/";
$webroot = "/~ist181861/bd/site"; 

$host = "db.ist.utl.pt";
$dbname="ist181861";
$user = "ist181861";
$password = "nckl3837";

//TODO: Inicializar ligacao a db
try{
	$db = new PDO("mysql:host=$host;dbname=$dbname", $user, $password);
	$db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
} catch(PDOException $e) {
    echo("<p>ERROR: {$e->getMessage()}</p>");
    exit("Aborting..."); 
}

//Code to emulate used http request methods on html5 forms:
$valid_methods = ["GET", "POST", "PUT", "DELETE", "PUT", "HEAD"];
$METHOD = $_SERVER['REQUEST_METHOD'];
if (($_SERVER['REQUEST_METHOD']=="HEAD") && isset($_GET["_method"]) && in_array($_GET["_method"], $valid_methods)) {
    $METHOD = $_GET["_method"];
  }
if (($_SERVER['REQUEST_METHOD']=="GET") && isset($_GET["_method"]) && in_array($_GET["_method"], $valid_methods)) {
  $METHOD = $_GET["_method"];
}
if (($_SERVER['REQUEST_METHOD']=="POST") && isset($_POST["_method"]) && in_array($_POST["_method"], $valid_methods)) {
  $METHOD = $_POST["_method"];
}
?>