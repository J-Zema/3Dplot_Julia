
using Plots
using Base.Iterators

function rysuj(funkcja::Function, funkcja_string::String, dziedzina, wybór_dziedziny::Bool, punkt::Tuple, h=0.000001)
    """
    Funkcja, która rysuje powierzchnię podanej funkcji dwóch zmiennych oraz płaszczyznę styczną w podanym punkcie.
    Argumenty:
        funkcja - funkcja dwóch zmiennych,
        funkcja_string - ta sama funkcja dwóch zmiennych podana jako String,
        dziedzina - dziedzina funkcji podana jako ((a,b),(c,d)) - gdy obszar ma być prostokątem lub ((k,l), r) - gdy obszar ma być kołem,
                    gdzie (a,b) - zakres x, (c,d) - zakres y, (k, l) - środek okręgu, r - promień,
        wybór_dziedziny - true oznacza obszar prostokątny, false oznacza obszar koła,
        punkt - punkt styczności podany jako krotka, np. (1,2)
        h - parametr przybliżający pochodne cząstkowe, domyślnie jst to wartość 0.000001,
    """

    #obszar prostokątu
    if wybór_dziedziny == true

        #dziedzina
        as=LinRange(dziedzina[1][1],dziedzina[1][2],1000)
        bs=LinRange(dziedzina[2][1],dziedzina[2][2],1000)

        #sprawdzenie, czy punkt należy do dziedzieny
        if (punkt[1]>=dziedzina[1][1] && punkt[1]<=dziedzina[1][2]) && (punkt[2]>=dziedzina[2][1] && punkt[2]<=dziedzina[2][2])

            #punkt styczności
            x₀ = punkt[1]
            y₀ = punkt[2]
            z₀ = funkcja(punkt[1], punkt[2])
            P = (x₀, y₀, round(z₀, digits=2))

            #pochodne cząstkowe w P
            ∂x = (funkcja(x₀ + h, y₀) - funkcja(x₀ - h, y₀))/2h
            ∂y = (funkcja(x₀, y₀ + h) - funkcja(x₀, y₀ - h))/2h

            #płaszczyzna styczna
            #π = ∂x(x - x₀) + ∂y(y - y₀) + z₀
            z(a,b) = (∂x)*a - (∂x)*x₀ + (∂y)*b - (∂y)*y₀ + z₀

            #funkcja
            h = funkcja.(as, bs')

            surface(as, bs, h,
            camera = (80,30),
            dpi=130,
            xlim = dziedzina[1],
            ylim = dziedzina[2],
            legend = :best,
            label = "f(x,y)",
            ylabel = "y",
            xlabel = "x",
            zlabel = "z",
            title = "$funkcja_string" ) |>display

            #płaszczyzna styczna
            surface!(as, bs, z.(as, bs'),
                color = cgrad([:red,:yellow,:green]),
                title = "f(x,y) i płaszczyzna styczna"
                )
            #punkt styczności
            scatter!([x₀],[y₀],[z₀],
            color = "black",
            label = "P = $P" )


        else
           return error("Podany punkt nie należy do dziedziny funkcji!")
        end

    #obszar kołą
    elseif wybór_dziedziny == false

        #równanie okręgu
        c(x,y) = ((x - dziedzina[1][1])^2 + (y - dziedzina[1][2])^2)

        #jeśli punkt należy do dziedziny
        if (c(punkt[1],punkt[2]) <=(dziedzina[2])^2)

            #kąt
            θ = LinRange(0,2*pi, 50)

            #promień
            r = LinRange(0,dziedzina[2],50)

            #tablica tupli (kąt, promień)
            c=Array{Tuple{Float64,Float64},1}()

            #iloczyn kartezjański
            #nie było mowy, że nie można używać pętli
            for (i,j) in product(θ, r)
                append!(c, Ref((i,j)))
            end

            #tablice osobno na θ i na promień
            α = Array{Float64}(undef, length(c))
            R = Array{Float64}(undef, length(c))

            for i in 1:length(c)
                α[i]=c[i][1]
                R[i]=c[i][2]
            end

            #współrzędne parametryczne
            X = R .*(cos.(α)) .+dziedzina[1][1]
            Y = R .*(sin.(α)) .+dziedzina[1][2]

            #funkcja
            l = funkcja.(X, Y)

            #może nie wygląda idealnie, ale bardzo długo zajęło mi dojście do tego i nie wiem czy da się inaczej
            surface(X, Y, l,
            camera = (30,40),
            title = "$funkcja_string",
            xlim = (dziedzina[1][1] - dziedzina[2], dziedzina[1][1] + dziedzina[2]),
            ylim = (dziedzina[1][2] - dziedzina[2], dziedzina[1][2] + dziedzina[2]),
            )

            #punkt styczności
            x₀ = punkt[1]
            y₀ = punkt[2]
            z₀ = funkcja(punkt[1], punkt[2])
            P = (x₀, y₀, round(z₀, digits=2))

            #pochodne cząstkowe w P
            ∂x = (funkcja(x₀ + h, y₀) - funkcja(x₀ - h, y₀))/2h
            ∂y = (funkcja(x₀, y₀ + h) - funkcja(x₀, y₀ - h))/2h

            poch_x = (funkcja(x₀ + h, y₀) - funkcja(x₀ - h, y₀))/2h
            poch_y = (funkcja(x₀, y₀ + h) - funkcja(x₀, y₀ - h))/2h

            #płaszczyzna styczna
            W(a,b) = (poch_x)*a - (poch_x)*x₀ + (poch_y)*b - (poch_y)*y₀ + z₀

            surface!(X, Y, W.(X, Y),
            color = cgrad([:blue,:yellow,:green]),
            title = "f(x,y) i płaszczyzna styczna")

            #punkt styczności
            scatter!([x₀],[y₀],[z₀],
            color = "red",
            label = "P = $P")

            #zadanie nie było proste

        else
            return error("Podany punkt nie należy do dziedziny funkcji!")
        end
    else
        return error("Niepoprawny wybór dziedziny!")
    end
end

g = f(x,y) = exp(-x.^2 - y.^2)
h =((-4,4),(-4,4))
okrąg1 = ((1,1), 2)
okrąg2 = ((0,0), 2)
C = (0,0)
A = (0.5,1)
k = q(x,y) = sin(x*y)

rysuj(g,"f(x,y) = exp(-x^2 - y^2)", h, true, C) |>display
rysuj(g,"f(x,y) = exp(-x^2 - y^2)", okrąg1, false, A)
rysuj(g,"f(x,y) = exp(-x^2 - y^2)", okrąg2, false, A)
rysuj(k,"f(x,y) = sin(x*y)", h, true, A) |>display
