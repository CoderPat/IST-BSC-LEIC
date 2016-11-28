<?php 
  require_once "func/init.php";
  include "template/head.php"; 
  include "template/navbar.php";
?>

<?php 
      include "model/edificio.php";
      include "model/espaco.php";
      include "model/posto.php";
      include "model/oferta.php";
      include "model/reserva.php";
      include "view/table.php"
      $table = edificio_getall();
      view_table($table);
      
      $table = espaco_getall();
      view_table($table);
      
      $table = posto_getall();
      view_table($table);
      
      $table = oferta_getall();
      view_table($table);
      
      $table = reserva_getall();
      view_table($table);
?>

<?php include "template/foot.php"; ?>
