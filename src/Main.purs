module Main where

import Data.Argonaut
import Data.Either
import Data.Maybe
import Prelude
import Text.TypeScript.Parse
import Text.TypeScript.GenForeign

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"
  -- log $ generateDocumentationForInputString program
  let parsed = tryParseSourceFile program
  -- let stringed = encodeJson <$> parsed
  -- let stringied2 = stringify <$> stringed
  -- let hushed = hush stringied2
  -- let maybed = fromMaybe "" hushed
  log "test1"
  -- log maybed
  log "test2"
  log $ either show identity $ stringify <$> encodeJson <$> tryParseSourceFile program
  log $ either show identity $ genModulePs "three" <$> tryParseSourceFile program

program = """
import { Camera } from './Camera';

export function someFunc(type1?: number, type2: Vector2, type3: Vector2[], type4: SomeClass<Type1>, type5: SomeClass2<Type2<Type3>>);
export const enum DirectionExported {
        Up,
        Down,
        Left,
        Right
    }

export interface ReadonlyTextRange {
    readonly pos: number;
    readonly end: number;
    readonly arrayValue: number[];
}
/**
 * Camera with perspective projection.
 *
 * @source https://github.com/mrdoob/three.js/blob/master/src/cameras/PerspectiveCamera.js
 */
export class PerspectiveCamera extends Camera {

	/**
	 * @param [fov=50] Camera frustum vertical field of view. Default value is 50.
	 * @param [aspect=1] Camera frustum aspect ratio. Default value is 1.
	 * @param [near=0.1] Camera frustum near plane. Default value is 0.1.
	 * @param [far=2000] Camera frustum far plane. Default value is 2000.
	 */
	constructor( fov?: number, aspect?: number, near?: number, far?: number );

	type: 'PerspectiveCamera';

	readonly isPerspectiveCamera: true;

	/**
	 * @default 1
	 */
	zoom: number;

	/**
	 * Camera frustum vertical field of view, from bottom to top of view, in degrees.
	 * @default 50
	 */
	fov: number;

	/**
	 * Camera frustum aspect ratio, window width divided by window height.
	 * @default 1
	 */
	aspect: number;

	/**
	 * Camera frustum near plane.
	 * @default 0.1
	 */
	near: number;

	/**
	 * Camera frustum far plane.
	 * @default 2000
	 */
	far: number;

	/**
	 * @default 10
	 */
	focus: number;

	/**
	 * @default null
	 */
	view: null | {
		enabled: boolean;
		fullWidth: number;
		fullHeight: number;
		offsetX: number;
		offsetY: number;
		width: number;
		height: number;
	};

	/**
	 * @default 35
	 */
	filmGauge: number;

	/**
	 * @default 0
	 */
	filmOffset: number;

	setFocalLength( focalLength: number ): void;
	getFocalLength(): number;
	getEffectiveFOV(): number;
	getFilmWidth(): number;
	getFilmHeight(): number [];

	/**
	 * Sets an offset in a larger frustum. This is useful for multi-window or multi-monitor/multi-machine setups.
	 * For example, if you have 3x2 monitors and each monitor is 1920x1080 and the monitors are in grid like this:
	 *
	 *		 +---+---+---+
	 *		 | A | B | C |
	 *		 +---+---+---+
	 *		 | D | E | F |
	 *		 +---+---+---+
	 *
	 * then for each monitor you would call it like this:
	 *
	 *		 const w = 1920;
	 *		 const h = 1080;
	 *		 const fullWidth = w * 3;
	 *		 const fullHeight = h * 2;
	 *
	 *		 // A
	 *		 camera.setViewOffset( fullWidth, fullHeight, w * 0, h * 0, w, h );
	 *		 // B
	 *		 camera.setViewOffset( fullWidth, fullHeight, w * 1, h * 0, w, h );
	 *		 // C
	 *		 camera.setViewOffset( fullWidth, fullHeight, w * 2, h * 0, w, h );
	 *		 // D
	 *		 camera.setViewOffset( fullWidth, fullHeight, w * 0, h * 1, w, h );
	 *		 // E
	 *		 camera.setViewOffset( fullWidth, fullHeight, w * 1, h * 1, w, h );
	 *		 // F
	 *		 camera.setViewOffset( fullWidth, fullHeight, w * 2, h * 1, w, h ); Note there is no reason monitors have to be the same size or in a grid.
	 *
	 * @param fullWidth full width of multiview setup
	 * @param fullHeight full height of multiview setup
	 * @param x horizontal offset of subcamera
	 * @param y vertical offset of subcamera
	 * @param width width of subcamera
	 * @param height height of subcamera
	 */
	setViewOffset(
		fullWidth: number,
		fullHeight: number,
		x: number,
		y: number,
		width: number,
		height: number
	): void;
	clearViewOffset(): void;

	/**
	 * Updates the camera projection matrix. Must be called after change of parameters.
	 */
	updateProjectionMatrix(): void;
	toJSON( meta?: any ): any;

	/**
	 * @deprecated Use {@link PerspectiveCamera#setFocalLength .setFocalLength()} and {@link PerspectiveCamera#filmGauge .filmGauge} instead.
	 */
	setLens( focalLength: number, frameHeight?: number ): void;

}
"""

program1 = """
import { Vector2 } from './Vector2';

// Math //////////////////////////////////////////////////////////////////////////////////

export class Box2 {

	constructor( min?: Vector2, max?: Vector2, testArray: Vector2[] );

	/**
	 * @default new THREE.Vector2( + Infinity, + Infinity )
	 */
	min: Vector2;

	/**
	 * @default new THREE.Vector2( - Infinity, - Infinity )
	 */
	max: Vector2;

	set( min: Vector2, max: Vector2 ): Box2;
	setFromPoints( points: Vector2[] ): Box2;
	setFromCenterAndSize( center: Vector2, size: Vector2 ): Box2;
	clone(): this;
	copy( box: Box2 ): this;
	makeEmpty(): Box2;
	isEmpty(): boolean;
	getCenter( target: Vector2 ): Vector2;
	getSize( target: Vector2 ): Vector2;
	expandByPoint( point: Vector2 ): Box2;
	expandByVector( vector: Vector2 ): Box2;
	expandByScalar( scalar: number ): Box2;
	containsPoint( point: Vector2 ): boolean;
	containsBox( box: Box2 ): boolean;
	getParameter( point: Vector2, target: Vector2 ): Vector2;
	intersectsBox( box: Box2 ): boolean;
	clampPoint( point: Vector2, target: Vector2 ): Vector2;
	distanceToPoint( point: Vector2 ): number;
	intersect( box: Box2 ): Box2;
	union( box: Box2 ): Box2;
	translate( offset: Vector2 ): Box2;
	equals( box: Box2 ): boolean;
	/**
	 * @deprecated Use {@link Box2#isEmpty .isEmpty()} instead.
	 */
	empty(): any;
	/**
	 * @deprecated Use {@link Box2#intersectsBox .intersectsBox()} instead.
	 */
	isIntersectionBox( b: any ): any;

}
"""

program2 = """
import { Camera } from './Camera';

export class PerspectiveCamera extends Camera {

        constructor(name : Array<Animal<Dog>, Animal<Sheep>, number>, age: void);
	setFocalLength( focalLength: Vector2 [], ocalLength: number [] ): void;
	
	setFocalLength2( focalLength: number ): number[];
	setFocalLength3( focalLength: number ): Array<number, Vector2, boolean>;
	setFocalLength4( focalLength: number ): number[][];
}
"""
