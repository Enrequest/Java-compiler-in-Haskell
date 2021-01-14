class A {
   int result;
   int start;
   int end;
   int mid;
   int sqr;
   boolean m;
   int sqrt(int x){
        m = true && m || false;
   	start = 1;
   	end = x/2;
   	if (x < 2){
   		return x;
   	}
   	while (start < end){
   		mid = (start + end) / 2;
   		sqr = mid * mid;
   		
   		if (sqr == x){
   			return mid;
   		} else if (sqr < x) {
   			start = mid + 1;
   			result = mid;
   		} else {
   			end = mid - 1;
   		}
   	}
   	return result;
   }
   //int m3(int x, boolean y);
   /*
   int desde;
   int hasta;
   int contador;
   int resultado;
   int m1(){
   	desde = 0;
 	hasta = 10;
 	contador = desde;
 	contador2 = desde;
 	resultado = 0;
 	while(contador < hasta){
 		if(contador % 2 == 0){
 			resultado = resultado + contador;
 		}
 		else{
 			contador2 = contador2 + 1;
 		}
 		contador = contador + 1;
 	}
 	//return resultado;
   }
   */
}

