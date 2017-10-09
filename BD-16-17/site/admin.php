<?php 
  require_once "func/init.php";
  $title = "Painel de administração";
  include $root . "template/head.php"; 
  include $root . "template/navbar.php";
?>
<div class="jumbotron">
   <div class="container">      
      <p><a class='btn btn-primary btn-lg' href='pages/edificio' role='button'>Ver edifícios &raquo;</a></p>

      <p><a class='btn btn-primary btn-lg' href='pages/espaco' role='button'>Ver espacos &raquo;</a></p>

      <p><a class='btn btn-primary btn-lg' href='pages/posto' role='button'>Ver postos &raquo;</a></p>

      <p><a class='btn btn-primary btn-lg' href='pages/oferta' role='button'>Ver ofertas &raquo;</a></p>

      <p><a class='btn btn-primary btn-lg' href='pages/reserva' role='button'>Ver reservas &raquo;</a></p>
   </div>
</div>


<?php include $root . "template/foot.php"; ?>
