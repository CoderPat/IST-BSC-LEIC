
$(document).ready(function() {

	var bebidas_alcoolicas ={
		'Bellini' : ['Bellini', "100 mililitros de suco de pêssego e 50 mililitros de espumante", 14, 'http://s3-eu-west-1.amazonaws.com/jamieoliverprod/_int/rdb2/upload/1205_1_1403269148_lrg.jpg', 1],
    'Bloody Mary' : ['Bloody Mary', "vodca, suco de tomate, suco de limão, sal, molho inglês, tabasco e pimenta", 18, 'http://realgirlskitchen.com/wp-content/uploads/2011/12/Bloodymary.jpg', 2],
    'Daiquiri' : ['Daiquiri', "45 mililitros de rum, 25 mililitros sumo de limão e 15 mililitros de sirup", 16, 'http://www.bevdig.com/uploads/2/6/7/4/26740497/2857864_orig.jpg', 3],
    'Caipirinha' : ['Caipirinha', "cachaça, limão-taiti não descascado, açúcar e gelo", 25, 'http://cucafrescaspirit.com/wp-content/files_mf/cache/th_e163919b020bc293009280a9f28b0587_cocktail_notes.png', 4]
	};

	var bebidas_nao_alcoolicas ={
		'Arizona Sunset' : ['Arizona Sunset', "Sumo", 3, 'http://www.stepbystep.com/wp-content/uploads/2013/05/Arizona-Sunset-Cocktail-Recipe-21-e1368612439188.jpg', 5],
    'Limonada' : ['Limonada', "Limao e açucar", 2, 'http://del.h-cdn.co/assets/cm/15/10/54f685750c488_-_the_ultimate_ketel_one_lemonade_hires-xl.jpg', 6],
		'Caipirinha' : ['Caipirinha', "cachaça, limão-taiti não descascado, açúcar e gelo", 25, 'http://cucafrescaspirit.com/wp-content/files_mf/cache/th_e163919b020bc293009280a9f28b0587_cocktail_notes.png', 7]

	};


	var $aperitivos = $('.aperitivos')
	var $shop1 = $('.shop1')
	var $shop = $('.shop')
	var $cart = $('.cart-items')

	for(var item in bebidas_alcoolicas){
		var itemName = bebidas_alcoolicas[item][0],
		itemDescription = bebidas_alcoolicas[item][1],
		itemPrice = bebidas_alcoolicas[item][2],
		itemImg = bebidas_alcoolicas[item][3],
		itemId = bebidas_alcoolicas[item][4],
		$template = $($('#productTemplate').html());

		$template.find('.merdas').text(itemName);
    $template.find('.p').text(itemDescription);
    $template.find('.price').text('€' + itemPrice);
    $template.css('background-image', 'url(' + itemImg + ')');

    $template.data('id', itemId);
    $template.data('name', itemName);
    $template.data('price', itemPrice);
    $template.data('image', itemImg);

    $shop.append($template);
	}

	for(var item in bebidas_nao_alcoolicas){
	  var itemName = bebidas_nao_alcoolicas[item][0],
	  itemDescription = bebidas_nao_alcoolicas[item][1],
	  itemPrice = bebidas_nao_alcoolicas[item][2],
	  itemImg = bebidas_nao_alcoolicas[item][3],
	  itemId = bebidas_nao_alcoolicas[item][4],
	  $template = $($('#productTemplate').html());

	  $template.find('.merdas').text(itemName);
	  $template.find('.p').text(itemDescription);
	  $template.find('.price').text('€' + itemPrice);
	  $template.css('background-image', 'url(' + itemImg + ')');

	  $template.data('id', itemId);
	  $template.data('name', itemName);
	  $template.data('price', itemPrice);
	  $template.data('image', itemImg);

	  $shop1.append($template);
	}

	// Checks quantity of a cart item on input blur and updates total
	$('body').on('blur', '.cart-items input', function() {
		var $this = $(this),
				$item = $this.parents('li');
		if (+$this.val() === 0) {
			$item.remove();
		} else {
			calculateSubtotal($item);
		}
		updateCartQuantity();
		calculateAndUpdate();
	});

	// Checks quantity of a cart item on input blur and updates total
	// If quantity is zero, item is removed

	$('body').on('blur', '.cart-items input', function() {
		var $this = $(this),
				$item = $this.parents('li');
		if (+$this.val() === 0) {
			$item.remove();
		} else {
			calculateSubtotal($item);
		}
		updateCartQuantity();
		calculateAndUpdate();
	});


	// Add item from the shop to the cart
  // If item is already in the cart, +1 to quantity
  // If not, creates the cart item based on template

  $('body').on('click', '.product .add', function() {
    var items = $cart.children(),
        $item = $(this).parents('.product'),
        $template = $($('#cartItem').html()),
        $matched = null,
        quantity = 0;

    $matched = items.filter(function(index) {
      var $this = $(this);
      return $this.data('id') === $item.data('id');
    });

    if ($matched.length) {
      quantity = +$matched.find('.quantity').val() + 1;
      $matched.find('.quantity').val(quantity);
      calculateSubtotal($matched);
    } else {
      $template.find('.cart-product').css('background-image', 'url(' + $item.data('image') + ')');
      $template.find('h3').text($item.data('name'));
      $template.find('.subtotal').text('$' + $item.data('price'));

      $template.data('id', $item.data('id'));
      $template.data('price', $item.data('price'));
      $template.data('subtotal', $item.data('price'));

      $cart.append($template);
    }

    updateCartQuantity();
    calculateAndUpdate();
  });

	//checkout request

	$('.checkout').on('click', function () {
		var raw_items = $cart.children().toArray();
		add_to_order(raw_items);

		$("#order_placed_box").css("display", "block")
		$cart.empty();

		updateCartQuantity();
    	calculateAndUpdate();
	});

	// Calculates subtotal for an item

  function calculateSubtotal($item) {
    var quantity = $item.find('.quantity').val(),
        price = $item.data('price'),
        subtotal = quantity * price;
    $item.find('.subtotal').text('$' + subtotal);
    $item.data('subtotal', subtotal);
  }

	// Calculates and updates totals, taxes, shipping

  function calculateAndUpdate() {
    var subtotal = 0,
        items = $cart.children(),
        // shipping not applied if there are no items
        shipping = items.length > 0 ? 5 : 0,
        tax = 0;
    items.each(function(index, item) {
      var $item = $(item),
          price = $item.data('subtotal');
      subtotal += price;
    });
    $('.subtotalTotal span').text(formatDollar(subtotal));
	}
	//  Formats number into dollar format

  function formatDollar(amount) {
    return '€' + parseFloat(Math.round(amount * 100) / 100).toFixed(2);
  }

	//  Update the total quantity of items in notification, hides if zero

	function updateCartQuantity() {
		var quantities = 0,
				$cartQuantity = $('span.cart-quantity'),
				items = $cart.children();
		items.each(function(index, item) {
			var $item = $(item),
					quantity = +$item.find('.quantity').val();
			quantities += quantity;
		});
		if(quantities > 0){
			$cartQuantity.removeClass('empty');
		} else {
			$cartQuantity.addClass('empty');
		}
		$cartQuantity.text(quantities);
	}



	// Restrict the quantity input field to numbers only

	$('body').on('keypress', '.cart-items input', function (ev) {
			var keyCode = window.event ? ev.keyCode : ev.which;
			if (keyCode < 48 || keyCode > 57) {
				if (keyCode != 0 && keyCode != 8 && keyCode != 13 && !ev.ctrlKey) {
					ev.preventDefault();
				}
			}
		});


		// Trigger animation on Add to Cart button click

	  $('.addtocart').on('click', function () {
	    $(this).addClass('active');
	    setTimeout(function () {
	      $('.addtocart').removeClass('active');
	    }, 1000);
	  });


});

var input = document.getElementById('quant_num');

// When user clicks on one of the menu buttons in pedir page
function show_menu(x){
	var menu_section = document.getElementById("section_menu");
	var el_showned = document.getElementById(x);
	if(menu_section.style.right == "44%"){
		menu_section.style.right = 65 + "%";
		/*menu_section.style.top = 9 + "%";*/
		el_showned.style.display = "block";
		previous_displayed = el_showned;
	}
	else if(menu_section.style.right == "65%" && el_showned.style.display != "block"){
		previous_displayed.style.display = "none";
		el_showned.style.display = "block";
		previous_displayed = el_showned;
	}

	else {
		previous_displayed.style.display = "none";
		menu_section.style.right = 44 + "%";
		/*menu_section.style.top = 5 + "%";*/
	}
}


/*When the user cliks on a order*/

function list(id) {
    if(list_box.style.display == 'block')
       list_box.style.display = 'none';
    else
       list_box.style.display = 'block';
}


// When a user cliks on a item
function item(id) {
  if(id.style.display == 'block')
     id.style.display = 'none';
  else
     id.style.display = 'block';
}


function add(v){
  var x = input;
  var y = v;
  var z = x + y;
  document.getElementById("demo").innerHTML = z;
}
