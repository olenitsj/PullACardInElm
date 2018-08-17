<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js"></script>
$(document).ready(function () {

    function shuffle(array) {
        var counter = array.length,
            temp, index;

        // While there are elements in the array
        while (counter > 0) {
            // Pick a random index 
            index = Math.floor(Math.random() * counter);

            // Decrease counter by 1
            counter--;

            // And swap the last element with it
            temp = array[counter];
            array[counter] = array[index];
            array[index] = temp;
        }

        return array;
    }

    var images = ["Kaarten-GM1", "Kaarten-GM2", "Kaarten-GM3", "Kaarten-GM4", "Kaarten-GM5", "Kaarten-GM6", "Kaarten-GM7", "Kaarten-GM8", "Kaarten-GM9", "Kaarten-GM10", "Kaarten-GM11", "Kaarten-GM12", "Kaarten-GM13", "Kaarten-GF1", "Kaarten-GF2", "Kaarten-GF3", "KaartenGOV", "the-One", "Kaarten-GFF", "Kaarten-GMF", "Kaarten-levensbloem", "Kaarten-Agnihotra", "Kaarten-GF-Fotonengordel-web", "Kaarten-GF4", "Kaarten-GF5", "Kaarten-GF6", "Kaarten-GF7"];


    shuffle(images);

    jQuery.each(images, function (index, value) {
        var zindex = index + 2000;
        var zindex_str = "" + zindex;


        $("#spread").append('<li><img style="z-index: ' + zindex_str + '; -ms-transform: rotate(7deg); -webkit-transform: rotate(7deg); transform: rotate(7deg);" id="' + value + '" class="kaart" height="200" width="200" src="https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/LOGO-600x600.jpg"/></li>');
        return index < 6;


    });



    $(".kaart").on('mouseover', function () {
        $(this).animate({
            top: '+=3em'
        });
    });


    $(".kaart").on('mouseout', function () {
        $(this).animate({
            top: '-=3em'
        });

    });

    $(".kaart").one('click', function (event) {
        $(this).unbind('mouseout');
        $(this).unbind('mouseover');

        $(this).animate({
            top: '+=4em'
        }, "slow", function () {

            $(this).css({
                "-ms-transform": "rotate(0deg)",
                    "-webkit-transform": "rotate(0deg)",
                    "transform": "rotate(0deg)",
                	"margin-left": "5%"
            });

            var id_assig = event.target.id;
            var srcc = 'https://www.13grandmothersremedies.com/wp-content/uploads/2015/11/' + id_assig + '.jpg';
            $(this).animate().attr({
                src: srcc,
                width: "700em",
                height: "700em"
            });

            $("#canvas").prepend("<h2>U heeft deze kaart getrokken</h2> <br>");

            var GFgroep = ["Kaarten-GF1", "Kaarten-GF2", "Kaarten-GF3", "Kaarten-GF4", "Kaarten-GF5", "Kaarten-GF6", "Kaarten-GF7", "Kaarten-GFF"]
            var GMgroep = ["Kaarten-GM1", "Kaarten-GM2", "Kaarten-GM3", "Kaarten-GM4", "Kaarten-GM5", "Kaarten-GM6", "Kaarten-GM7", "Kaarten-GM8", "Kaarten-GM9", "Kaarten-GM10", "Kaarten-GM11", "Kaarten-GM12", "Kaarten-GM13", "Kaarten-GMF"]

            if ($.inArray(this.id, GFgroep) > -1) {
                $("#canvas").append("<a href='https://www.13grandmothersremedies.com/producten/de-grootvaderremedies/" + this.id + "' id='trek_een_kaart_koop_knop'>klik hier om de bijhorende remedie te bekijken </a> <br>");

            } else if ($.inArray(this.id, GMgroep) > -1) {
                $("#canvas").append("<a href='https://www.13grandmothersremedies.com/producten/de-grootmoederremedies/" + this.id + "' id='trek_een_kaart_koop_knop'>klik hier om de bijhorende remedie te bekijken </a> <br>");

            } else {
                $("#canvas").append("<a href='https://www.13grandmothersremedies.com/producten/aanvullende-remedies/" + this.id + "/' id='trek_een_kaart_koop_knop'>klik hier om de bijhorende remedie te bekijken </a> <br>");

            }





            $(this).parent().siblings("li").not($(this).parents()).slideUp("slow", function () {
                $(this).parent().siblings("li").not($(this).parents()).remove();



            });
        });
        $(this).animate({
            top: '-=8em'
        }, "slow");


    });
});
</script>






<div id="canvas" width="600px"><ul style="max-width:100%" id="spread"></ul>
</div>