%--------------------------------------------------------------%
%   Wumpus world agents                                        %
%   (C) 1998 Zdravko Markov                                    %
%--------------------------------------------------------------%

% knowledgable agent

agent1([],[~ breeze(X,Y) & ~stench(X,Y) -> [go_forward,
                                            turn(left),
                                            go_forward,
                                            go_forward,
                                            grab,
                                            turn(left),
                                            turn(left),
                                            go_forward,
                                            go_forward,
                                            turn(right),
                                            go_forward,
                                            climb]
          ]
      ).


% Reasoning agent

agent2([
% Reasoning knowledge
  stench(X,Y) # ~stench(X,Y) -> visited(X,Y), % mark (X,Y) as visited
  wumpus(X,Y) & (succ(X,X1) # succ(X1,X)) -> stench(X1,Y),
  wumpus(X,Y) & (succ(Y,Y1) # succ(Y1,Y)) -> stench(X,Y1),
  pit(X,Y) & (succ(X,X1) # succ(X1,X)) -> breeze(X1,Y),
  pit(X,Y) & (succ(Y,Y1) # succ(Y1,Y)) -> breeze(X,Y1),
  succ(1,2),
  succ(2,3),
  succ(3,4),
  ~wumpus(X,Y) & ~pit(X,Y) -> ok(X,Y),
  glitter(X,Y) -> gold_grabbed
       ],
       [
% Action rules
   light(X,Y) & gold_grabbed -> [climb],
   glitter(X,Y) -> [grab,turn(left),turn(left),go_forward],
   ~stench(X,Y) & ~breeze(X,Y) & ~bump -> [go_forward],
   heading(X,Y) & visited(X,Y) -> [go_forward,turn(right)],
   heading(X,Y) & ok(X,Y) -> [go_forward],
   stench(X,Y) # breeze(X,Y) # bump -> [turn(right)]
]).
