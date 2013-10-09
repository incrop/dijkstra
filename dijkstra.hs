import qualified Data.Map as M  
import Data.List (sort, group, insert)

-- Пример графа взят с http://optlab-server.sce.carleton.ca/POAnimations2007/DijkstrasAlgo.html
-- Начальный список переходов со стоимостями
input = [ ('O', 'A', 2)
        , ('O', 'B', 5)
        , ('O', 'C', 4)
        , ('A', 'B', 2)
        , ('A', 'D', 7)
        , ('A', 'F', 12)
        , ('B', 'C', 1)
        , ('B', 'D', 4)
        , ('B', 'E', 3)
        , ('C', 'E', 4)
        , ('D', 'E', 1)
        , ('D', 'T', 5)
        , ('E', 'T', 7)
        , ('F', 'T', 3)
        ]

initVertex = 'O'
goalVertex = 'T'

invertEdges = map (\(from, to, dist) -> (to, from, dist))

-- рёбра двунаправленные, поэтому к начальному списку переходов 
-- нужно добавить список обратных переходов
edges = input ++ invertEdges input

-- Инициализируем расстояния до вершин.
-- на вход принимаем начальную вершину и список переходов (откуда, куда, стоимость перехода)
-- на выходе - отсортированный список троек (начальная стоимость пути до вершины, вершина, путь)
initDistances :: (Ord v, RealFloat n) => v -> [(v, v, n)] -> [(n, v, [v])]
initDistances initVertex edges = (map head . group . sort) (recur [] edges)
  where recur acc [] = acc
        recur acc ((from, to, _) : others) = 
              recur (initDist from : initDist to : acc) others
                where initDist v = (if v == initVertex then 0 else 1/0, v, [])


-- Инициализируем рёбра.
-- на входе список переходов (откуда, куда, стоимость перехода)
-- на выходе мапа которая каждой вершине в соотвествие ставит мапу,
-- где ключи - соседние вершины, значения - стоимость перехода до соседа 
initNeighbors :: (Ord v, RealFloat n) => [(v, v, n)] -> M.Map v (M.Map v n)
initNeighbors edges = recur M.empty edges
  where recur neighborsMap [] = neighborsMap
        recur neighborsMap ((from, to, dist) : rest) = recur newNeighbors rest
          where newNeighbors = M.insert from (M.insert to dist oldNeighbors) neighborsMap
                oldNeighbors = M.findWithDefault M.empty from neighborsMap

-- Алгоритм Дейкстры.
-- на входе результаты выполнения двух пердыдущих функций и целевая вершина
-- на выходе МожетБыть пара (стоимость перехода, путь от начальной вершины до целевой)
dijkstra :: (Ord v, RealFloat n) => M.Map v (M.Map v n) -> [(n, v, [v])] -> v -> Maybe (n, [v])               
dijkstra _ [] _ = Nothing
dijkstra neighbors (firstUnvisited : restUnvisited) goalVertex = recur (removeMin firstUnvisited restUnvisited)
  where removeMin currMin [] = (currMin, [])
        removeMin currMin (curr : others) = 
          let newMin = min currMin curr
              notMin = max currMin curr
              (foundMin, othersNotMin) = removeMin newMin others
          in (foundMin, notMin : othersNotMin)
        recur ((currDistance, currVertex, currPath), unvisited)
          | currDistance == 1/0      = Nothing
          | currVertex == goalVertex = Just (currDistance, reverse (currVertex : currPath))
          | otherwise                = 
              case unvisited of 
                [] -> Nothing
                firstUnvisited : restUnvisited ->
                  recur (removeMin (recalcDistance firstUnvisited) (map recalcDistance restUnvisited))
                    where currNeighbors = M.findWithDefault M.empty currVertex neighbors
                          recalcDistance orig@(estDistance, vertex, _) = 
                            case M.lookup vertex currNeighbors of
                              Nothing -> orig
                              Just neighDistance 
                                | altDistance >= estDistance -> orig
                                | otherwise                  -> (altDistance, vertex, currVertex : currPath)
                                    where altDistance = currDistance + neighDistance

-- выводим в консоль результат работы алгоритма 
main = (putStrLn . show) (dijkstra (initNeighbors edges) (initDistances initVertex edges) goalVertex)
