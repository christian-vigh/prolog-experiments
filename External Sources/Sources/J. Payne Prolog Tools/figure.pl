
/*figures*/
/*test descriptions for evans program*/
/*Alan Bundy 26.10.79*/

problem1(Ans) :- evans(figa,figb,figc,[fig1,fig2,fig3,fig4,fig5],Ans).
problem2(Ans) :- evans(figa,figb,figc,[fig1,fig2,fig3,fig4a,fig5],Ans).
problem3(Ans) :- evans(figa,figb,figc,[fig1,fig2,fig3,fig5],Ans).


objects(figa,[tri1,tri2]).
relations(figa,[[inside,tri1,tri2]]).

objects(figb,[tri3]).
relations(figb,[]).

similarities(figa,figb,[[tri2,tri3,direct], [tri1,tri3,[scale,2]]]).

objects(figc,[square,circle]).
relations(figc,[[inside,square,circle]]).


objects(fig1,[circle2,circle3]).
relations(fig1,[[inside,circle2,circle3]]).
similarities(figc,fig1,[[circle,circle3,direct],
			[circle,circle2,[scale,half]]]).

objects(fig2,[square2]).
relations(fig2,[]).
similarities(figc,fig2,[[square,square2,direct]]).

objects(fig3,[tri4,circle4]).
relations(fig3,[[inside,tri4,circle4]]).
similarities(figc,fig3,[[circle,circle4,direct]]).

objects(fig4,[circle5]).
relations(fig4,[]).
similarities(figc,fig4,[[circle,circle5,direct]]).

objects(fig4a,[square3]).
relations(fig4a,[]).
similarities(figc,fig4a,[[square,square3,[scale,2]]]).

objects(fig5,[tri5]).
relations(fig5,[]).
similarities(figc,fig5,[]).


