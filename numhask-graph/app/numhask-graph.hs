{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import qualified Control.Foldl as L
import Control.Monad.Primitive (unsafeInlineIO)
import qualified Data.GraphViz as GV
import qualified Data.Map as Map
import qualified Data.Text as Text
import Diagrams.Backend.SVG (SVG, renderSVG)
import Diagrams.Prelude hiding (Loop, (<>))
import Diagrams.TwoD ()
import qualified Diagrams.TwoD.GraphViz as DGV
import qualified Diagrams.TwoD.Text
import Protolude as P

data Class
  = Magma
  | Unital
  | Associative
  | Commutative
  | Invertible
  | Idempotent
  | Monoidal
  | CMonoidal
  | Loop
  | Group
  | Abelian
  | Representable
  | Additive
  | AdditiveGroup
  | Multiplicative
  | MultiplicativeGroup
  | Distribution
  | Semiring
  | Ring
  | CRing
  | Field
  | ExpField
  | QuotientField
  | BoundedField
  | Basis
  | GroupBasis
  | Module
  | GroupModule
  | Banach
  | Hilbert
  | Foldable
  | TensorProduct
  | Integral
  | ToInteger
  | FromInteger
  | Metric
  | Normed
  | Signed
  | Epsilon
  deriving (Show, Eq, Ord)

data Family
  = Addition
  | Multiplication
  deriving (Show, Eq, Ord)

data Dependency = Dependency
  { _class :: Class
  , _dep :: Class
  , _op :: Maybe Family
  } deriving (Show, Eq, Ord)

dependencies :: [Dependency]
dependencies =
  [ Dependency Unital Magma Nothing
  , Dependency Associative Magma Nothing
  , Dependency Commutative Magma Nothing
  , Dependency Invertible Magma Nothing
  , Dependency Idempotent Magma Nothing
  , Dependency Monoidal Unital Nothing
  , Dependency Monoidal Associative Nothing
  , Dependency CMonoidal Unital Nothing
  , Dependency CMonoidal Associative Nothing
  , Dependency CMonoidal Commutative Nothing
  , Dependency Loop Unital Nothing
  , Dependency Loop Invertible Nothing
  , Dependency Group Unital Nothing
  , Dependency Group Invertible Nothing
  , Dependency Group Associative Nothing
  , Dependency Abelian Unital Nothing
  , Dependency Abelian Invertible Nothing
  , Dependency Abelian Associative Nothing
  , Dependency Abelian Commutative Nothing
  , Dependency Additive Commutative (Just Addition)
  , Dependency Additive Unital (Just Addition)
  , Dependency Additive Associative (Just Addition)
  , Dependency AdditiveGroup Additive (Just Addition)
  , Dependency AdditiveGroup Invertible (Just Addition)
  , Dependency Multiplicative Commutative (Just Multiplication)
  , Dependency Multiplicative Unital (Just Multiplication)
  , Dependency Multiplicative Associative (Just Multiplication)
  , Dependency MultiplicativeGroup Multiplicative (Just Multiplication)
  , Dependency MultiplicativeGroup Invertible (Just Multiplication)
  , Dependency Distribution Additive (Just Addition)
  , Dependency Distribution Magma (Just Multiplication)
  , Dependency Semiring Associative (Just Multiplication)
  , Dependency Semiring Unital (Just Multiplication)
  , Dependency Semiring Distribution Nothing
  , Dependency Ring Group (Just Addition)
  , Dependency Ring AdditiveGroup (Just Addition)
  , Dependency Ring Semiring Nothing
  , Dependency CRing Multiplicative (Just Multiplication)
  , Dependency CRing Ring Nothing
  , Dependency Field Group (Just Addition)
  , Dependency Field Group (Just Multiplication)
  , Dependency Field AdditiveGroup (Just Addition)
  , Dependency Field MultiplicativeGroup (Just Multiplication)
  , Dependency Field Distribution Nothing
  , Dependency ExpField Field Nothing
  , Dependency QuotientField Field Nothing
  , Dependency BoundedField Field Nothing
  , Dependency Integral Ring Nothing
  , Dependency FromInteger Ring Nothing
  , Dependency Basis Representable Nothing
  , Dependency GroupBasis Representable Nothing
  , Dependency Module Representable Nothing
  , Dependency GroupModule Additive Nothing
  , Dependency Banach Representable Nothing
  , Dependency Banach ExpField Nothing
  , Dependency Banach Normed Nothing
  , Dependency Hilbert Semiring Nothing
  , Dependency TensorProduct Hilbert Nothing
  , Dependency TensorProduct Multiplicative Nothing
  ]

fileSvg f s = renderSVG f (mkSizeSpec (Just <$> r2 s))

data Config = Config
  { _rectX :: Double
  , _rectY :: Double
  , _textScale :: Double
  , _arrowScale :: Double
  }

-- | Render an annotated graph as a diagram, given functions
--   controlling the drawing of vertices and of edges.  The first
--   function is given the label and location of each vertex. The
--   second function, for each edge, is given the label and location
--   of the first vertex, the label and location of the second vertex,
--   and the label and path corresponding to the edge.
boxes ps =
  zipWith
    (\p c ->
       place
         ((unitSquare # scaleX 50.0 # scaleY 25.0 # lc (sRGB 0.33 0.33 0.33) .
           opacity 0.3 <>
           (Diagrams.Prelude.text (Text.unpack $ show c) # scale 5.0)) #
          named c)
         p)
    (Map.elems ps :: [P2 Double])
    (Map.keys ps)

edge ::
     (Renderable (Path V2 Double) b)
  => Dependency
  -> QDiagram b V2 Double Any
  -> QDiagram b V2 Double Any
edge (Dependency to from Nothing) =
  connectOutside'
    (headStyle %~ fc (sRGB 0.33 0.33 0.33) . opacity 0.3 $ shaftStyle %~
     lc (sRGB 0.33 0.33 0.33) .
     opacity 0.3 $
     arrowHead .~
     dart $
     headLength .~
     8 $
     def)
    from
    to
edge (Dependency to from (Just Addition)) =
  connectOutside'
    (headStyle %~ fc red . opacity 0.5 $ shaftStyle %~ lc red . opacity 0.5 $
     arrowHead .~
     dart $
     headLength .~
     8 $
     def)
    from
    to
edge (Dependency to from (Just Multiplication)) =
  connectOutside'
    (headStyle %~ fc blue . opacity 0.5 $ shaftStyle %~ lc blue . opacity 0.5 $
     arrowHead .~
     dart $
     headLength .~
     8 $
     def)
    from
    to

ps cs ds =
  fst $ DGV.getGraph $ unsafeInlineIO $
  DGV.layoutGraph GV.Dot (DGV.mkGraph cs (toEdge <$> ds))
  where
    toEdge (Dependency to from wrapper) = (from, to, wrapper)

instance IsName Class

makeGraph ::
     ( (Renderable (Path V2 Double) b)
     , Renderable (Diagrams.TwoD.Text.Text Double) b
     )
  => Config
  -> [Class]
  -> [Dependency]
  -> QDiagram b V2 Double Any
makeGraph Config {} cs ds =
  L.fold (L.Fold (flip edge) (mconcat $ boxes (ps cs ds)) identity) ds

tensorProductClasses =
  [ Magma
  , Unital
  , Associative
  , Commutative
  , Additive
  , Multiplicative
  , Distribution
  , Semiring
  , Hilbert
  , TensorProduct
  ]

fieldClasses =
  [ Magma
  , Unital
  , Associative
  , Commutative
  , Invertible
  , Additive
  , AdditiveGroup
  , Multiplicative
  , Distribution
  , Semiring
  , Ring
  , CRing
  ]

main :: IO ()
main = do
  let gField = makeGraph (Config 3 30 10 1) fieldClasses dependencies
  fileSvg "other/field.svg" (600, 600) (gField :: QDiagram SVG V2 Double Any)
  let gHilbert = makeGraph (Config 3 30 10 1) tensorProductClasses dependencies
  fileSvg
    "other/tensor_product.svg"
    (600, 600)
    (gHilbert :: QDiagram SVG V2 Double Any)
