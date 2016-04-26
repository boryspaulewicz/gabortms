## -*- coding: utf-8 -*-

## TODO: catch triale = czas prezentacji 0, żeby sprawdzić, czy nie
## podają zbyt wysokiej pewności gdy nic nie ma.

## Kolejność zawsze gabor, skala. Gabor ma nałożony szum jednostajny.

## Dwa treningi: czasy standardowo 4 Trening odpowiedzi: 500 ms x 8
## prób Trening ze skalą: 4 czasy x 16 prób - zapisywanie danych już
## dla treningu ze skalą


if(interactive())source('~/cs/code/r/tasks/task/task.R')

## Globalne parametry zadania

FIXATION.TIME = 1500
POST.STIM.TIME = 32
LONELY.MASK.DURATION = 0
SCALE.MAX.DURATION = 100000
MAX.REACTION.TIME = 3000
FEEDBACK.TIME = 1000
FITTING.START = 50
TARGET.ACC = .8
block.length = 24

## Parametry rysowania gabora i skali

sigma = .01
f = 30
contrast = .8
scale.position = .65
mask.intensity = .85 ## minimalnie 0, maksymalnie .99 albo coś takiego

## Globalne obiekty graficzne

TXT$set.string("Proszę nacisnąć spację")
center(TXT, WINDOW)
FX = fixation(WINDOW, size = .02)
STIM = new(Text)
STIM$set.font(FONT)

i = new(Image)
mask = new(Image)
## Tworzymy tylko tyle obrazka, ile potrzeba
for(obj in c(i, mask)){
    obj$create(WINDOW$get.size()[1] * 6 * sigma, WINDOW$get.size()[1] * 6 * sigma, c(0, 0, 0))
}
## draw.sin(i, f = f, 45, sigma = sigma, contrast = contrast, mask = F)
## draw.sin(mask, f = f, 45, sigma = sigma, mask = 2) ## maska w postaci szumu
i.texture = new(Texture)
i.texture$create(i$size[1], i$size[2])
i.texture$update(i, 0, 0)
mask.texture = new(Texture)
mask.texture$create(mask$size[1], mask$size[2])
mask.texture$update(mask, 0, 0)
s = new(Sprite)
s$set.texture(i.texture, F)
center(s, WINDOW)
m = new(Sprite)
m$set.texture(mask.texture, F)
center(m, WINDOW)

## Funkcje pomocnicze, typu rysowanie bodźców

draw.stim = function(side){
    if(side == 'left'){
        s$set.rotation(-90)
    }else{
        s$set.rotation(0)
    }
    WINDOW$draw(s)
}

## Dwa klawisze w kluczu reakcyjnym

KEYS <<- c(Key.Left, Key.Right)

dane = model = NULL
new.contrast = contrast ## na początku domyślny
trial.code = function(trial, side = 'left', contrast = .5, duration = 64, withscale = 1, feedback = 0, scale = 'pas', stage = 'unknown',
                      show.leftright = 1){
    ## Kod specyficzny dla zadania
    ## ...
    ## Szablon
    if(trial == 1){
        dane <<- data.frame(contrast = NA, acc = rep(NA, 400))
        state = 'press-space'
    }else if((trial %% block.length) == 0){
        state = 'break'
    }else{ state = 'show-fixation' }
    ## Rysujemy bodziec
    if(trial > FITTING.START)contrast = new.contrast
    draw.sin(i, f = f, 45, sigma = sigma, contrast = contrast, mask = 3,
             mask.intensity = mask.intensity)
    i.texture$update(i, 0, 0)
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        if(KEY.PRESSED[Key.Escape + 1] > start)return(NULL)
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            WINDOW$clear(c(.5, .5, .5))
            TXT$set.string("Naciśnij spację aby rozpocząć zadanie")
            WINDOW$draw(center.win(TXT))
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'break' = {
            WINDOW$clear(c(.5, .5, .5))
            TXT$set.string("Krótka przerwa - odpocznij. Aby kontynuować, naciśnij spację")
            WINDOW$draw(center.win(TXT))
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            WINDOW$clear(c(.5, .5, .5))
            ## Losowa pozycja myszki
            mouse.set.position(c(runif(1, min = .2, max = .8), .5) * WINDOW$get.size())
            ## Punkt fiksacji
            lapply(FX, WINDOW$draw)
            WINDOW$set.mouse.cursor.visible(F)
            WINDOW$display()
            state = 'clear-fixation'
            fixation.start = CLOCK$time
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$set.mouse.cursor.visible(F)
                WINDOW$clear(c(.5, .5, .5))
                WINDOW$display()
                state = 'show-gabor'
                fixation.cleared = CLOCK$time
            }
        }, 'show-gabor' = {
            WINDOW$clear(c(.5, .5, .5))
            draw.stim(side)
            WINDOW$display()
            stim.onset = CLOCK$time
            state = 'gabor-present'
        }, 'gabor-present' = {
            if((CLOCK$time - stim.onset) > duration){
                ## Znikamy gabora
                WINDOW$clear(c(.5, .5, .5))
                WINDOW$display()
                stim.cleared = CLOCK$time
                state = 'post-gabor'
            }
        }, 'post-gabor' = { ## tu rysujemy maskę
            if((CLOCK$time - stim.cleared) > POST.STIM.TIME){
                ## WINDOW$draw(m) ##! Bez maski po bodźcu
                ## WINDOW$display()
                mask.onset = CLOCK$time
                scale.rt = scale.value = -1
                mp = 666
                state = 'mask-present'
            }
        }, 'mask-present' = {
            if((CLOCK$time - mask.onset) > LONELY.MASK.DURATION)state = 'show-leftright'
        }, 'show-leftright' = {
            WINDOW$clear(c(.5, .5, .5))
            ## WINDOW$draw(m) ##! Bez maski po bodźcu
            if(show.leftright){
                TXT$set.string("LEWO     PRAWO")
                center(TXT, WINDOW)
                TXT$set.position(c(WINDOW$get.size()[1] / 2, WINDOW$get.size()[2] * scale.position))
                WINDOW$draw(TXT)
            }
            WINDOW$display()
            leftright.onset = CLOCK$time
            state = 'measure-reaction'
        }, 'measure-reaction' = {
            if(any(BUTTON.PRESSED[1:2] > stim.onset) || ((CLOCK$time - stim.onset) > MAX.REACTION.TIME)){
                response = which(BUTTON.PRESSED[1:2] > stim.onset)
                rt = BUTTON.PRESSED[response] - stim.onset
                acc = as.numeric(response == c(left = 1, right = 2)[side])
                if((CLOCK$time - stim.onset) > MAX.REACTION.TIME){
                    rt = MAX.REACTION.TIME
                    acc = 2
                }
                if(withscale == 1){
                    scale.onset = CLOCK$time
                    state = 'draw-scale'
                }else{
                    if(feedback == 1){
                        feedback.onset = CLOCK$time
                        state = 'feedback'
                    }else{
                        state = 'done'
                    }
                }
            }
        }, 'draw-scale' = {
            WINDOW$set.mouse.cursor.visible(T)
            if(((CLOCK$time - scale.onset) > SCALE.MAX.DURATION) ||
               (BUTTON.PRESSED[1] > scale.onset)){
                    scale.rt = BUTTON.PRESSED[1] - scale.onset
                    scale.value = mp[1]
                    state = 'done'
            }else{
                WINDOW$clear(c(.5, .5, .5))
                ## WINDOW$draw(m)
                switch(as.character(scale),
                       'pas' = {
                           mp = draw.scale(list(M = c('Nic nie widziałem', 'Widziałem niewyraźnie', 'Widziałem dość wyraźnie', 'Widziałem bardzo wyraźnie'),
                                                K = c('Nic nie widziałam', 'Widziałam niewyraźnie', 'Widziałam dość wyraźnie', 'Widziałam bardzo wyraźnie'))[[USER.DATA$gender]],
                                           background.color = c(.5, .5, .5), position = scale.position, draw.bar = F)
                       },
                       'cpas' = {
                           mp = draw.scale(list(M = c('Nic nie widziałem', 'Widziałem niewyraźnie', 'Widziałem dość wyraźnie', 'Widziałem bardzo wyraźnie'),
                                                K = c('Nic nie widziałam', 'Widziałam niewyraźnie', 'Widziałam dość wyraźnie', 'Widziałam bardzo wyraźnie'))[[USER.DATA$gender]],
                                           background.color = c(.5, .5, .5), position = scale.position, draw.bar = T)
                       },
                       'ias' = {
                           mp = draw.scale(rep("", 11), gradient  = T,
                                           background.color = c(.5, .5, .5), position = scale.position, draw.bar = F)
                       },
                       'cs' = {
                           mp = draw.scale(list(M = c('Nic nie widziałem', 'Widziałem bardzo wyraźnie'),
                                                K = c('Nic nie widziałam', 'Widziałam bardzo wyraźnie'))[[USER.DATA$gender]],
                                           background.color = c(.5, .5, .5), position = scale.position, draw.bar = T)
                       })
                WINDOW$display()
            }
        }, 'feedback' = {
            if((CLOCK$time - feedback.onset) < FEEDBACK.TIME){
                WINDOW$clear(c(.5, .5, .5))
                TXT$set.string(c('Źle', 'Dobrze', 'Za późno')[acc + 1])
                WINDOW$draw(center.win(TXT))
                WINDOW$display()
            }else{
                state = 'done'
            }
        }, 'done' = {
            WINDOW$clear(c(.5, .5, .5))
            WINDOW$display()
            ## Zapisujemy dotychczasowe dane i dopasowujemy model do kalibracji, ale tylko na etapie testowym
            dane[trial, c('contrast', 'acc')] <<- c(contrast, acc)
            if((trial > FITTING.START) & (stage == 'test')){
                model <<- glm(acc ~ contrast, dane[dane$acc %in% 0:1,], family = binomial)
                if(coef(model)[2] > 0){
                    new.contrast = contrast
                }else{ 
                    new.contrast <<- optimize(function(x)(abs(TARGET.ACC - binomial()$linkinv(coef(model)[1] + x * coef(model)[2]))),
                                        c(0, 1))$minimum
                }
            }
            return(list(scalert = scale.rt, scalevalue = scale.value,
                        rt = rt, acc = acc, contrastopt = new.contrast))
        })
    }
}

docx.instr = function(file, ask.if.done = T){
    res = 'Tak'
    while(res == 'Tak'){
        system(sprintf('libreoffice --view --invisible %s', file))
        if(ask.if.done){
            res = gui.show.instruction('Czy chcesz jeszcze raz zobaczyć ostatnią instrukcję?', c('Nie', 'Tak'))
        }else{ break }
    }
}

TASK.NAME <<- 'gabortms'

cnd = 'pas'

docx.instr(c(pas = 'InstrukcjaPAS.docx', cpas = 'InstrukcjaCPAS.docx', ias = 'InstrukcjaIAS.docx', cs = 'InstrukcjaCS.docx')[cnd])

## ## Wybieramy na początek zawsze PAS-a, nie pytamy o dane osobowe
## cnd = 'pas' ## może być też 'cpas', 'ias', albo 'cs'

## Tak się uruchamia w danym momencie określoną instrukcję
docx.instr('Instrukcja0.docx')

gui.user.data()

docx.instr('Instrukcja1.docx')

## Trening 1, 500 ms 8 prób bez skali
run.trials(trial.code, condition = cnd, expand.grid(side = c('left', 'right'), scale = cnd,
                           withscale = 0, feedback = 1, duration = 500, contrast = contrast),
           b = 4)

docx.instr('Instrukcja2.docx')

## Trening 2, 16 prób ze skalą, czas 32
run.trials(trial.code, condition = cnd, expand.grid(side = c('left', 'right'), scale = cnd, stage = 'trening2',
                           withscale = 1, feedback = 0, duration = 32, contrast = contrast, show.leftright = 0),
           b = 8, record.session = T)

docx.instr('Instrukcja3.docx')

## Etap właściwy, 320 prób, czas 32, z kalibracją
run.trials(trial.code, condition = cnd, expand.grid(side = c('left', 'right'), scale = cnd, stage = 'test',
                           withscale = 1, feedback = 0, duration = 32, contrast = c(.1, .4), show.leftright = 0),
           b = 8, n = 10, record.session = T)

docx.instr('Instrukcja4.docx', F)

if(!interactive())quit("no")
