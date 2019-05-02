using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MonteCarlo
{
    public class MonteCarlo
    {
        static Random r = new Random();

        public virtual dynamic GetRandom()
        {
            return 0;
        }

        public static void MonteCarlo_Variation()
        {
            int n = 20000; //n is the number of simulations
            double l_three_d = 0; //length of a line
            double[] distancesThreeD = new double[n]; // here the distance is stored in an array for each iteration of n

            BoxMullerNormal b = new BoxMullerNormal();
            //repeat the experiment n (20k) times
            for (int i = 0; i < n; i++)
            {
                //get a point (x1,x2,y1,y2) at random
                double x1 = b.GetRandom();
                double x2 = b.GetRandom();
                double y1 = b.GetRandom();
                double y2 = b.GetRandom();
                double z1 = b.GetRandom();
                double z2 = b.GetRandom();
                
                //3D
                double result2 = ThreeD(x1, x2, y1, y2, z1, z2);
                l_three_d = result2; //get length of a 3d line
                distancesThreeD[i] = l_three_d;
            }

            #region average
            double averageOfLineThreeD = Queryable.Average(distancesThreeD.AsQueryable());
            #endregion

            #region standard dev
            double sum = distancesThreeD.Sum(d => Math.Pow(d - averageOfLineThreeD, 2));
            double sd_3d = Math.Sqrt((sum) / (distancesThreeD.Count() - 1));
            #endregion

            #region confidence interval
            double marginOfError_3d = 2.58 * (sd_3d / Math.Sqrt(n));
            double lowerEndOfInterval_3d = averageOfLineThreeD - marginOfError_3d;//lower end of the interval is average - margin of error 
            double UpperEndOfInterval_3d = averageOfLineThreeD + marginOfError_3d;//and upper end is average + margin of error
            #endregion

            #region display results
            Console.WriteLine("3D Results: \n");
            Console.WriteLine("Average of 3d: " + averageOfLineThreeD);
            Console.WriteLine("Standard Deviation 3d: " + sd_3d);
            Console.WriteLine("μ: " + averageOfLineThreeD + " ± " + marginOfError_3d);
            Console.WriteLine("lower end of interval = " + lowerEndOfInterval_3d);
            Console.WriteLine("upper end of interval = " + UpperEndOfInterval_3d);
            Console.WriteLine("You can be 99% confident that the population mean (μ) falls between " + lowerEndOfInterval_3d + " and " + UpperEndOfInterval_3d);
            #endregion

            Console.WriteLine("\nPress any key to continue..... \n");

        }

        public static string FindMaxError(double e, double alpha, int n=100, int repeats=1)
        {
            Console.WriteLine("Default problem size = " + n);
            double l_three_d = 0; //length of a line
            double[] distancesThreeD = new double[n]; // here the distance is stored in an array for each iteration of n
            double c = 0;
            int k = 0;
            BoxMullerNormal b = new BoxMullerNormal();

            //repeat the experiment n times
            for (int i = 0; i < n; i++)
            {
                //get a point (x1,x2,y1,y2,z1,z2) at random
                double x1 = b.GetRandom();
                double x2 = b.GetRandom();
                double y1 = b.GetRandom();
                double y2 = b.GetRandom();
                double z1 = b.GetRandom();
                double z2 = b.GetRandom();
                
                //3D
                double result2 = ThreeD(x1, x2, y1, y2, z1, z2);
                l_three_d = result2; //get length of a 3d line
                distancesThreeD[i] = l_three_d;
            }

            double averageOfLineThreeD = Queryable.Average(distancesThreeD.AsQueryable());
            double sum = distancesThreeD.Sum(d => Math.Pow(d - averageOfLineThreeD, 2));
            double sd_3d = Math.Sqrt((sum) / (distancesThreeD.Count() - 1));
            //double sd_3d = Math.Sqrt(distancesThreeD.Average(v => Math.Pow(v - averageOfLineThreeD, 2)));


            c = alpha * (sd_3d/ Math.Sqrt(n));
            
            if(c > e)
            {
                Console.WriteLine("c is less than e, so try again...");
                k = 1;
                n = (int)Math.Ceiling(Math.Pow((alpha * sd_3d / e), 2) + k);
                repeats++;
                return FindMaxError(e, alpha, n, repeats);
            }
            else
            {
                Console.WriteLine("c is greater than e, Done!");
                #region confidence interval
                //3D
                double marginOfError_3d = alpha * (sd_3d / Math.Sqrt(n));
                double lowerEndOfInterval_3d = averageOfLineThreeD - marginOfError_3d;//lower end of the interval is average - margin of error 
                double UpperEndOfInterval_3d = averageOfLineThreeD + marginOfError_3d;//and upper end is average + margin of error
                Console.WriteLine("μ: " + averageOfLineThreeD + " ± " + marginOfError_3d);
                Console.WriteLine("lower end of interval = " + lowerEndOfInterval_3d);
                Console.WriteLine("upper end of interval = " + UpperEndOfInterval_3d);
                Console.WriteLine($"You can be {alpha}% confident that the population mean (μ) falls between" + lowerEndOfInterval_3d + " and " + UpperEndOfInterval_3d);
                #endregion
                Console.WriteLine("Repeats: " + repeats);
                return "Max error: " + c + " , Ideal n = " + n;
            }
        }

        public static void SimpleMonteCarlo()
        {
            
            int n = 3000; //n is the number of simulations
            double l = 0;  //length of a line
            double l_three_d = 0; //length of a line
            double[] distances = new double[n]; // here the distance is stored in an array for each iteration of n
            double[] distancesThreeD = new double[n]; // here the distance is stored in an array for each iteration of n
            
            //2D --> repeat the experiment n (20k) times
            for (int i = 0; i < n; i++)
            {
                //get a point (x1,x2,y1,y2) at random
                double x1 = r.NextDouble();
                double x2 = r.NextDouble();
                double y1 = r.NextDouble();
                double y2 = r.NextDouble();

                //2D
                double result = TwoD(x1, x2, y1, y2);
                l = result;
                distances[i] = l;
            }
            
            //3D --> repeat the experiment n (20k) times
            for (int i = 0; i < n; i++)
            {
                //get a point (x1,x2,y1,y2) at random
                double x1 = r.NextDouble();
                double x2 = r.NextDouble();
                double y1 = r.NextDouble();
                double y2 = r.NextDouble();
                double z1 = r.NextDouble();
                double z2 = r.NextDouble();

                //3D
                double result2 = ThreeD(x1, x2, y1, y2, z1, z2);
                l_three_d = result2;
                distancesThreeD[i] = l_three_d;
            }
           
            #region average
            double averageOfLine = Queryable.Average(distances.AsQueryable());
            double averageOfLineThreeD = Queryable.Average(distancesThreeD.AsQueryable());
            #endregion

            #region standard dev
            //2D
            double sum2D = distancesThreeD.Sum(d => Math.Pow(d - averageOfLine, 2));
            double sd = Math.Sqrt((sum2D) / (distances.Count() - 1));

            //3D
            double sum3D = distancesThreeD.Sum(d => Math.Pow(d - averageOfLineThreeD, 2));
            double sd_3d = Math.Sqrt((sum3D) / (distancesThreeD.Count() - 1));
            #endregion  

            #region confidence interval
            //2D
            //x = average, theta = sd, and n = 20k
            //2.58 * (sd / sqrt(20k) ==[141.421356237]) = 2.58 * sd (0.sd) = margin of error
            double marginOfError = 2.58 * (sd / Math.Sqrt(n));
            double lowerEndOfInterval = averageOfLine - marginOfError;//lower end of the interval is average - margin of error 
            double UpperEndOfInterval = averageOfLine + marginOfError;//and upper end is average + margin of error

            //3D
            double marginOfError_3d = 2.58 * (sd_3d / Math.Sqrt(n));
            double lowerEndOfInterval_3d = averageOfLineThreeD - marginOfError_3d;//lower end of the interval is average - margin of error 
            double UpperEndOfInterval_3d = averageOfLineThreeD + marginOfError_3d;//and upper end is average + margin of error
            #endregion


            Console.WriteLine("n = " + n);
            Console.WriteLine("2D Results: \n");
            Console.WriteLine("Average: " + averageOfLine);
            Console.WriteLine("Standard Deviation: " + sd);
            Console.WriteLine("μ: " + averageOfLine + " ± " + marginOfError);
            Console.WriteLine("lower end of interval = " + lowerEndOfInterval);
            Console.WriteLine("upper end of interval = " + UpperEndOfInterval);
            Console.WriteLine("You can be 99% confident that the population mean (μ) falls between" + lowerEndOfInterval + " and " + UpperEndOfInterval);


            Console.WriteLine("\n****\n");


            Console.WriteLine("3D Results: \n");
            Console.WriteLine("Average of 3d: " + averageOfLineThreeD);
            Console.WriteLine("Standard Deviation 3d: " + sd_3d);
            Console.WriteLine("μ: " + averageOfLineThreeD + " ± " + marginOfError_3d);
            Console.WriteLine("lower end of interval = " + lowerEndOfInterval_3d);
            Console.WriteLine("upper end of interval = " + UpperEndOfInterval_3d);
            Console.WriteLine("You can be 99% confident that the population mean (μ) falls between" + lowerEndOfInterval_3d + " and " + UpperEndOfInterval_3d);

            Console.WriteLine("\n****\n");
            Console.WriteLine("Press any key to continue ...");

        }

        /// <summary>
        /// Returns the length 𝑙, of a line in 2-dimensions, using the Euclidean distance,
        /// Which is given by this equation : Math.Sqrt(Math.Pow((x1 - x2), 2) + Math.Pow((y1 - y2), 2)); 
        /// </summary>
        /// <param name="x1"></param>
        /// <param name="x2"></param>
        /// <param name="y1"></param>
        /// <param name="y2"></param>
        /// <returns></returns>
        private static double TwoD(double x1, double x2, double y1, double y2)
        {
            return Math.Sqrt(Math.Pow((x1 - x2), 2) + Math.Pow((y1 - y2), 2));
        }


        /// <summary>
        /// Returns the length 𝑙, of a line in 3-dimensions, using the Euclidean distance,
        /// Which is given by this equation : Math.Sqrt(Math.Pow((x1 - x2), 2) + Math.Pow((y1 - y2), 2) + Math.Pow((z1 - z2), 2));
        /// </summary>
        /// <param name="x1"></param>
        /// <param name="x2"></param>
        /// <param name="y1"></param>
        /// <param name="y2"></param>
        /// <param name="z1"></param>
        /// <param name="z2"></param>
        /// <returns></returns>
        private static double ThreeD(double x1, double x2, double y1, double y2, double z1, double z2)
        {
            return Math.Sqrt(Math.Pow((x1 - x2), 2) + Math.Pow((y1 - y2), 2) + Math.Pow((z1 - z2), 2));
        }

    }
}
