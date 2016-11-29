<?php 
  require_once "func/init.php";
  $title = "Painel de administração";
  include $root . "template/head.php"; 
  include $root . "template/navbar.php";
?>

<?php 
      include "model/edificio.php";
      include "model/espaco.php";
      include "model/posto.php";
      include "model/oferta.php";
      include "model/reserva.php";
      include "view/table.php";
      
      $table = edificio_getall();
      view_table($table);
      
      echo "<p><a class='btn btn-primary btn-lg' href='pages/edificio' role='button'>Ver edifícios &raquo;</a></p>";

      $table = espaco_getall();
      view_table($table);
      
      $table = posto_getall();
      view_table($table);
      
      $table = oferta_getall();
      view_table($table);
      
      $table = reserva_getall();
      view_table($table);
?>

<?php include $root . "template/foot.php"; ?>
