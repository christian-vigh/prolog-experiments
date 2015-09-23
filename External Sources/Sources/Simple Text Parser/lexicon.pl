/**
 * Simple PROLOG text parser
 *
 * Dimitri PISSARENKO, University of Derby in Austria
 * May 15, 2002
 *
 * In this file, the lexicon of the parser is defined.
 **/

period(period('.')) --> ['.'].
semicolon(semicolon(';')) --> [';'].
comma(comma(',')) --> [','].

n(n(war)) --> [war].
n(n(continuation)) --> [continuation].
n(n(politics)) --> [politics].
v(v(is)) --> [is].
p(p(of)) --> [of].
det(det(the)) --> [the].
p(p(in)) --> [in].
pn(pn(this)) --> [this].
n(n(sense)) --> [sense].
c(c(and)) --> [and].
pn(pn(itself)) --> [itself].
det(det(a)) -->  [a].
adj(adj(political)) --> [political].
n(n(action)) --> [action].
p(p(since)) --> [since].
adj(adj(ancient)) --> [ancient].
n(n(times)) --> [times].
adv(adv(there)) --> [there].
v(v(has)) --> [has].
adv(adv(never)) --> [never].
v(v(been)) --> [been].
subj(subj(that)) --> [that].
v(v(did)) --> [did].
adv(adv(not)) --> [not].
v(v(have)) --> [have].
n(n(character)) --> [character].
c(c(but)) --> [but].
pn(pn(its)) --> [its].
pn(pn(own)) --> [own].
adj(adj(particular)) --> [particular].
n(n(characteristics)) --> [characteristics].
pn(pn(it)) --> [it].
v(v(cannot)) --> [cannot].
v(v(be)) --> [be].
v(v(equated)) --> [equated].
p(p(with)) --> [with].
adj(adj(general)) --> [general].
p(p(by)) --> [by].
adj(adj(other)) --> [other].
n(n(means)) --> [means].
p(p(when)) --> [when].
v(v(develops)) --> [develops].
p(p(to)) --> [to].
adj(adj(certain)) --> [certain].
n(n(stage)) --> [stage].
p(p(beyond)) --> [beyond].
pn(pn(which)) --> [which].
v(v(proceed)) --> [proceed].
adj(adj(usual)) --> [usual].
v(v(breaks)) --> [breaks].
part(part(out)) --> [out].
part(part(to)) --> [to].
v(v(sweep)) --> [sweep].
n(n(obstacles)) --> [obstacles].
p(p(from)) --> [from].
n(n(way)) --> [way].
adv(adv(when)) --> [when].
n(n(obstacle)) --> [obstacle].
v(v(removed)) --> [removed].
pn(pn(our)) --> [our].
n(n(aim)) --> [aim].
v(v(attained)) --> [attained].
v(v(will)) --> [will].
v(v(stop)) --> [stop].
subj(subj(if)) --> [if].
adv(adv(completely)) --> [completely].
v(v(swept)) --> [swept].
adv(adv(away)) --> [away].
v(v(continue)) --> [continue].
p(p(till)) --> [till].
adv(adv(fully)) --> [fully].
adj(adj(accomplished)) --> [accomplished].
v(v(can)) --> [can].
subj(subj(therefore)) --> [therefore].
v(v(said)) --> [said].
p(p(without)) --> [without].
n(n(bloodshed)) --> [bloodshed].
c(c(while)) --> [while].
