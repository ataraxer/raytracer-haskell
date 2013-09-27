import Linal.Vector
import Ray
import Camera


main = do
	putStrLn "Bad-Ass Haskell Raytracer v0.1"
	let image = render 640 480
	putStrLn "Done!"

render w h = do
	x <- [0..w-1]
	y <- [0..h-1]
	return $ renderPixel (x, y) (w, h)

renderPixel c s = traceRay $ rayToPixel sceneCamera c s
	where sceneCamera = camera (Vec3 0 0 0) (Vec3 0 0 0)

rayToPixel sceneCamera (x, y) (w, h) = Ray cameraPosition direction
	where
		epsilon = 0.5
		xAmount = (x + epsilon) / w
		yAmount = ((h - y) + epsilon) / h
		direction = Vec3 0 0 0
		cameraPosition = position sceneCamera

traceRay ray = 0
